package App::LintPrereqs;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any qw($log);

use Config::IniFiles;
use File::Find;
use File::Which;
use Filename::Backup qw(check_backup_filename);
use Scalar::Util 'looks_like_number';
use Version::Util qw(version_gt version_ne);

our %SPEC;
require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(lint_prereqs);

sub _scan_prereqs {
    my %args = @_;

    my $scanner = do {
        if ($args{lite}) {
            require Perl::PrereqScanner::Lite;
            my $scanner = Perl::PrereqScanner::Lite->new;
            $scanner->add_extra_scanner('Moose');
            $scanner->add_extra_scanner('Version');
            $scanner;
        } else {
            require Perl::PrereqScanner;
            Perl::PrereqScanner->new;
        }
    };
    require File::Find;
    my @files;
    find(
        sub {
            return unless -f;
            return if check_backup_filename(filename=>$_);
            push @files, "$File::Find::dir/$_";
        },
        (grep {-d} (
            "t", "xt", "lib", "bin", "script", "scripts",
            #"sample", "samples", "example", "examples" # decidedly not included
            #"share", # decidedly not included
        ))
    );
    my %res;
    for my $file (@files) {
        my $scanres = $scanner->scan_file($file);
        unless ($scanres) {
            $log->tracef("Scanned %s, got nothing", $file);
        }
        my $reqs = $scanres->{requirements};
        $log->tracef("Scanned %s, got: %s", $file, [keys %$reqs]);
        #$log->tracef("TMP:reqs=%s", $reqs);
        for my $req (keys %$reqs) {
            my $v = $reqs->{$req}{minimum}{original};;
            if (exists $res{$req}) {
                $res{$req} = $v if version_gt($v, $res{$req});
            } else {
                $res{$req} = $v;
            }
        }
    }
    %res;
}

$SPEC{lint_prereqs} = {
    v => 1.1,
    summary => 'Check extraneous/missing prerequisites in dist.ini',
    description => <<'_',

Check `[Prereqs / *]` (as well as `OSPrereqs`, `Extras/lint-prereqs/Assume-*`)
sections in your `dist.ini` against what's actually being used in your Perl code
(using `Perl::PrereqScanner::Lite`) and what's in Perl core list of modules.
Will complain if your prerequisites are not actually used, or already in Perl
core. Will also complain if there are missing prerequisites.

Designed to work with prerequisites that are manually written. Does not work if
you use AutoPrereqs.

Sometimes there are prerequisites that you know are used but can't be detected
by scan_prereqs, or you want to include anyway. If this is the case, you can
instruct lint_prereqs to assume the prerequisite is used.

    ;!lint-prereqs assume-used # even though we know it is not currently used
    Foo::Bar=0
    ;!lint-prereqs assume-used # we are forcing a certain version
    Baz=0.12

Sometimes there are also prerequisites that are detected by scan_prereqs, but
you know are already provided by some other modules. So to make lint-prereqs
ignore them:

    [Extras / lint-prereqs / assume-provided]
    Qux::Quux=0

_
    args => {
        perl_version => {
            schema => ['str*'],
            summary => 'Perl version to use (overrides scan_prereqs/dist.ini)',
        },
        lite => {
            schema => ['bool*'],
            default => 1,
            summary => 'Use Perl::PrereqScanner::Lite instead of Perl::PrereqScanner',
            description => <<'_',

Lite is faster but it still misses detecting some modules, so it's not the
default.

_
        },
    },
    deps => {
        prog => 'scan_prereqs',
    },
};
sub lint_prereqs {
    my %args = @_;

    (-f "dist.ini")
        or return [412, "No dist.ini found. ".
                       "Are you in the right dir (dist top-level)? ".
                           "Is your dist managed by Dist::Zilla?"];

    my $cfg = Config::IniFiles->new(-file => "dist.ini", -fallback => "ALL");
    $cfg or return [
        500, "Can't open dist.ini: ".join(", ", @Config::IniFiles::errors)];

    my %mods_from_ini;
    my %assume_used;
    my %assume_provided;
    for my $section (grep {
        m!^(
              osprereqs \s*/\s* .+ |
              osprereqs(::\w+)+ |
              prereqs (?: \s*/\s* \w+)? |
              extras \s*/\s* lint[_-]prereqs \s*/\s* assume-(?:provided|used)
          )$!ix}
                         $cfg->Sections) {
        for my $param ($cfg->Parameters($section)) {
            my $v   = $cfg->val($section, $param);
            my $cmt = $cfg->GetParameterComment($section, $param) // "";
            #$log->tracef("section=$section, param=$param, v=$v, cmt=$cmt");
            $mods_from_ini{$param}   = $v unless $section =~ /assume-provided/;
            $assume_provided{$param} = $v if     $section =~ /assume-provided/;
            $assume_used{$param}     = $v if     $section =~ /assume-used/ ||
                $cmt =~ /^;!lint-prereqs\s+assume-used\b/m;
        }
    }
    $log->tracef("mods_from_ini: %s", \%mods_from_ini);
    $log->tracef("assume_used: %s", \%assume_used);
    $log->tracef("assume_provided: %s", \%assume_provided);

    # assume package names from filenames, should be better and scan using PPI
    my %pkgs;
    find({
        #no_chdir => 1,
        wanted => sub {
            return unless /\.pm$/;
            my $pkg = $File::Find::dir;
            #$log->errorf("TMP:pkg=%s",$pkg);
            $pkg =~ s!^lib/?!!;
            $pkg =~ s!/!::!g;
            $pkg .= (length($pkg) ? "::" : "") . $_;
            $pkg =~ s/\.pm$//;
            $pkgs{$pkg}++;
        },
    }, "lib");
    $log->tracef("Packages: %s", \%pkgs);

    my %mods_from_scanned = _scan_prereqs(lite=>$args{lite});
    $log->tracef("mods_from_scanned: %s", \%mods_from_scanned);

    if ($mods_from_ini{perl} && $mods_from_scanned{perl}) {
        if (version_ne($mods_from_ini{perl}, $mods_from_scanned{perl})) {
            return [500, "Perl version from dist.ini ($mods_from_ini{perl}) ".
                        "and scan_prereqs ($mods_from_scanned{perl}) mismatch"];
        }
    }

    my $perlv; # min perl v to use (& base corelist -v on), in x.yyyzzz format
    if ($args{perl_version}) {
        $log->tracef("Will assume perl %s (via perl_version argument)",
                     $args{perl_version});
        $perlv = $args{perl_version};
    } elsif ($mods_from_ini{perl}) {
        $log->tracef("Will assume perl %s (via dist.ini)",
                     $mods_from_ini{perl});
        $perlv = $mods_from_ini{perl};
    } elsif ($mods_from_scanned{perl}) {
        $log->tracef("Will assume perl %s (via scan_prereqs)",
                     $mods_from_scanned{perl});
        $perlv = $mods_from_scanned{perl};
    } else {
        $log->tracef("Will assume perl %s (from running interpreter's \$^V)",
                     $^V);
        if ($^V =~ /^v(\d+)\.(\d+)\.(\d+)/) {
            $perlv = sprintf("%d\.%03d%03d", $1, $2, $3)+0;
        } elsif (looks_like_number($^V)) {
            $perlv = $^V;
        } else {
            return [500, "Can't parse \$^V ($^V)"];
        }
    }

    my %core_mods;
    my $clpath = which("corelist")
        or return [412, "Can't find corelist in PATH"];
    my @clout = `corelist -v $perlv`;
    if ($?) {
        my $clout = join "", @clout;
        return [500, "corelist doesn't recognize perl version $perlv"]
            if $clout =~ /has no info on perl /;
        return [500, "Can't execute corelist command successfully"];
    }
    for (@clout) {
        chomp;
        /^([\w:]+)(?:\s+(\S+))?\s*$/ or next;
        #do {
        #    warn "Invalid line from $clpath: $_, skipped";
        #    next;
        #};
        $core_mods{$1} = $2 // 0;
    }
    $log->tracef("core modules in perl $perlv: %s", \%core_mods);

    my @errs;
    for my $mod (keys %mods_from_ini) {
        my $v = $mods_from_ini{$mod};
        next if $mod eq 'perl';
        $log->tracef("Checking mod from dist.ini: %s (%s)", $mod, $v);
        my $incorev = $core_mods{$mod};
        if (defined($incorev) && version_gt($incorev, $v)) {
            push @errs, {
                module  => $mod,
                error   => "Core in perl $perlv ($incorev) but ".
                    "mentioned in dist.ini ($v)",
                remedy  => "Remove in dist.ini or lower perl version ".
                    "requirement",
            };
        }
        my $scanv = $mods_from_scanned{$mod};
        if (defined($scanv) && $scanv != 0 && version_ne($v, $scanv)) {
            push @errs, {
                module  => $mod,
                error   => "Version mismatch between dist.ini ($v) ".
                    "and from scanned_prereqs ($scanv)",
                remedy  => "Fix either the code or version in dist.ini",
            };
        }
        unless (defined($scanv) || exists($assume_used{$mod})) {
            push @errs, {
                module  => $mod,
                error   => "Unused but listed in dist.ini",
                remedy  => "Remove from dist.ini",
            };
        }
    }

    for my $mod (keys %mods_from_scanned) {
        next if $mod eq 'perl';
        my $v = $mods_from_scanned{$mod};
        $log->tracef("Checking mod from scanned: %s (%s)", $mod, $v);
        if (exists $core_mods{$mod}) {
            my $incorev = $core_mods{$mod};
            if ($v != 0 && !$mods_from_ini{$mod} && version_gt($v, $incorev)) {
                push @errs, {
                    module  => $mod,
                    error   => "Version requested $v (from scan_prereqs) is ".
                        "higher than bundled with perl $perlv ($incorev)",
                    remedy  => "Specify in dist.ini with version=$v",
                };
            }
            next;
        }
        next if exists $pkgs{$mod};
        unless (exists($mods_from_ini{$mod}) ||
                    exists($assume_provided{$mod})) {
            push @errs, {
                module  => $mod,
                error   => "Used but not listed in dist.ini",
                remedy  => "Put '$mod=$v' in dist.ini",
            };
        }
    }

    my $rfopts = {
        table_column_orders  => [[qw/module error remedy/]],
    };
    my $resmeta = {
        "cmdline.exit_code" => @errs ? 500-300:0,
        result_format_options => {text=>$rfopts, "text-pretty"=>$rfopts},
    };
    [200, @errs ? "Extraneous/missing dependencies" : "OK", \@errs, $resmeta];
}

1;
#ABSTRACT:

=head1 SYNOPSIS

 # Use via lint-prereqs CLI script

=cut
