package App::LintPrereqs;

use 5.010001;
use strict;
use warnings;
use Log::Any qw($log);

use Config::IniFiles;
use File::Find;
use File::Which;
use Sort::Versions;
use Scalar::Util 'looks_like_number';

our %SPEC;
require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(lint_prereqs);

# VERSION

$SPEC{lint_prereqs} = {
    v => 1.1,
    summary => 'Check extraneous/missing prerequisites in dist.ini',
    description => <<'_',

Check [Prereqs / *] sections in your dist.ini against what's actually being used
in your Perl code (using Perl::PrereqScanner) and what's in Perl core list of
modules. Will complain if your prerequisites are not actually used, or already
in Perl core. Will also complain if there are missing prerequisites.

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
        m!^(prereqs|extras \s*/\s* lint[_-]prereqs \s*/\s*
              assume-(?:provided|used))!ix}
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

    my %mods_from_scanned;
    my $sppath = "scan_prereqs";
    my $spcmd = "$sppath --combine .";
    $spcmd .= " t/*.t" if <t/*.t>;
    $spcmd .= " bin/*" if <bin/*>;
    $spcmd .= " examples/*" if <examples/*>;
    for (`$spcmd`) {
        chomp;
        /^([\w:]+)\s*=\s*(.+)/ or do {
            warn "Invalid line from $sppath: $_, skipped";
            next;
        };
        $mods_from_scanned{$1} = $2;
    }
    $log->tracef("mods_from_scanned: %s", \%mods_from_scanned);

    if ($mods_from_ini{perl} && $mods_from_scanned{perl}) {
        if (versioncmp($mods_from_ini{perl}, $mods_from_scanned{perl})) {
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
        next if $mod eq 'perl';
        $log->tracef("Checking mod from dist.ini: %s", $mod);
        if (exists($core_mods{$mod}) &&
                versioncmp($core_mods{$mod}, $mods_from_ini{$mod}) >= 0) {
            push @errs, {
                module  => $mod,
                version => $mods_from_ini{$mod},
                message => "Core in perl $perlv but mentioned"};
        }
        if (exists($mods_from_scanned{$mod}) && $mods_from_scanned{$mod} != 0 &&
                versioncmp($mods_from_ini{$mod}, $mods_from_scanned{$mod})) {
            push @errs, {
                module  => $mod,
                version => $mods_from_ini{$mod},
                message => "Version mismatch (".
                    "$mods_from_scanned{$mod} from scan_prereqs)"};
        }
        unless (exists($mods_from_scanned{$mod}) ||
                    exists($assume_used{$mod})) {
            push @errs, {
                module  => $mod,
                version => $mods_from_ini{$mod},
                message => "Unused but listed in dist.ini"};
        }
    }

    for my $mod (keys %mods_from_scanned) {
        next if $mod eq 'perl';
        my $v = $mods_from_scanned{$mod};
        $log->tracef("Checking mod from scanned: %s (%s)", $mod, $v);
        if (exists $core_mods{$mod}) {
            my $incorev = $core_mods{$mod};
            if ($v != 0 && versioncmp($incorev, $v) == -1) {
                push @errs, {
                    module  => $mod,
                    version => $v,
                    message => "Version requested $v (from scan_prereqs) is ".
                        "higher than bundled with perl $perlv ($incorev)"};
            }
            next;
        }
        next if exists $pkgs{$mod};
        unless (exists($mods_from_ini{$mod}) ||
                    exists($assume_provided{$mod})) {
            push @errs, {
                module  => $mod,
                version => $mods_from_scanned{$mod},
                message => "Used but not listed in dist.ini"};
        }
    }

    my $rfopts = {
        table_column_orders  => [[qw/module message/]],
    };
    my $resmeta = {
        "cmdline.exit_code" => @errs ? 500-300:0,
        result_format_options => {text=>$rfopts, "text-pretty"=>$rfopts},
    };
    [200, @errs ? "Extraneous/missing dependencies" : "OK", \@errs, $resmeta];
}

1;
#ABSTRACT: Check extraneous/missing prerequisites in dist.ini

=head1 SYNOPSIS

 # Use via lint-prereqs CLI script

=cut
