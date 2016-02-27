package App::LintPrereqs;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any::IfLOG qw($log);

use Config::IOD;
use File::Find;
use File::Which;
use Filename::Backup qw(check_backup_filename);
use Module::CoreList::More;
use Scalar::Util 'looks_like_number';
use Sort::Sub qw(prereq_ala_perlancar);
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
    my %files; # key=phase, val=[file, ...]

    {
        $files{Runtime} = [];
        my @dirs = (grep {-d} (
            "lib", "bin", "script", "scripts",
            #"sample", "samples", "example", "examples" # decidedly not included
            #"share", # decidedly not included
        ));
        last unless @dirs;
        find(
            sub {
                return unless -f;
                return if check_backup_filename(filename=>$_);
                push @{$files{Runtime}}, "$File::Find::dir/$_";
            },
            @dirs
        );
    }

    {
        my @dirs = (grep {-d} ("t", "xt"));
        last unless @dirs;
        find(
            sub {
                return unless -f;
                return if check_backup_filename(filename=>$_);
                return unless /\.(t|pl|pm)$/;
                push @{$files{Test}}, "$File::Find::dir/$_";
            },
            @dirs
        );
    }

    my %res; # key=phase, value=hash of {mod=>version , ...}
    for my $phase (keys %files) {
        $res{$phase} = {};
        for my $file (@{$files{$phase}}) {
            my $scanres = $scanner->scan_file($file);
            unless ($scanres) {
                $log->tracef("Scanned %s, got nothing", $file);
            }
            my $reqs = $scanres->{requirements};
            $log->tracef("Scanned %s, got: %s", $file, [keys %$reqs]);
            #$log->tracef("TMP:reqs=%s", $reqs);
            for my $req (keys %$reqs) {
                my $v = $reqs->{$req}{minimum}{original};;
                if (exists $res{$phase}{$req}) {
                    $res{$phase}{$req} = $v
                        if version_gt($v, $res{$phase}{$req});
                } else {
                    $res{$phase}{$req} = $v;
                }
            }
        } # for file
    } # for phase

    # create a merged list of prereqs from any phase
    for my $phase (keys %files) {
        $res{Any} = {};
        for my $req (keys %{ $res{$phase} }) {
            my $v = $res{$phase}{$req};
            if (exists $res{Any}{$req}) {
                $res{Any}{$req} = $v
                    if version_gt($v, $res{Any}{$req});
            } else {
                $res{Any}{$req} = $v;
            }
        }
    }

    %res;
}

$SPEC{lint_prereqs} = {
    v => 1.1,
    summary => 'Check extraneous/missing/incorrect prerequisites in dist.ini',
    description => <<'_',

lint-prereqs can improve your prereqs specification in `dist.ini` by reporting
prereqs that are extraneous (specified but unused), missing (used/required but
not specified), or incorrect (mismatching version between what's specified in
`dist.ini` vs in source code, incorrect phase like test prereqs specified in
runtime, etc).

Checking actual usage of prereqs is done using `Perl::PrereqScanner` (or
`Perl::PrereqScanner::Lite`).

Sections that will be checked for prereqs include `[Prereqs / *]`, as well as
`OSPrereqs`, `Extras/lint-prereqs/Assume-*`. Designed to work with prerequisites
that are manually written. Does not work if you use AutoPrereqs (using
AutoPrereqs basically means that you do not specify prereqs and just use
whatever modules are detected by the scanner.)

Sometimes there are prerequisites that you know are used but can't be detected
by the scanner, or you want to include anyway. If this is the case, you can
instruct lint_prereqs to assume that the prerequisite is used.

    ;!lint_prereqs assume-used "even though we know it is not currently used"
    Foo::Bar=0

    ;!lint_prereqs assume-used "we are forcing a certain version"
    Baz=0.12

Sometimes there are also prerequisites that are detected by scan_prereqs, but
are false positives (`Perl::PrereqScanner::Lite` sometimes does this because its
parser is simpler) or you know are already provided by some other modules. So to
make lint-prereqs ignore them:

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
            default => 0,
            summary => 'Use Perl::PrereqScanner::Lite instead of Perl::PrereqScanner',
            "summary.alt.bool.not" =>
                'Use Perl::PrereqScanner instead of Perl::PrereqScanner::Lite',
            description => <<'_',

Lite is faster but it might still miss detecting some modules.

_
        },
        core_prereqs => {
            schema => ['bool*'],
            default => 1,
            summary => 'Whether or not prereqs to core modules are allowed',
            description => <<'_',

If set to 0 (the default), will complain if there are prerequisites to core
modules. If set to 1, prerequisites to core modules are required just like other
modules.

_
        },
    },
};
sub lint_prereqs {
    my %args = @_;

    (-f "dist.ini")
        or return [412, "No dist.ini found. ".
                       "Are you in the right dir (dist top-level)? ".
                           "Is your dist managed by Dist::Zilla?"];

    my $ct = do {
        open my($fh), "<", "dist.ini" or die "Can't open dist.ini: $!";
        local $/;
        ~~<$fh>;
    };
    return [200, "Not run (no-lint-prereqs)"] if $ct =~ /^;!no[_-]lint[_-]prereqs$/m;

    my $ciod = Config::IOD->new(
        allow_duplicate_key => 0,
        ignore_unknown_directive => 1,
    );

    my $cfg = $ciod->read_string($ct);

    my %mods_from_ini;
    my %assume_used;
    my %assume_provided;
    for my $section (grep {
        m!^(
              osprereqs \s*/\s* .+ |
              osprereqs(::\w+)+ |
              prereqs (?: \s*/\s* \w+)? |
              extras \s*/\s* lint[_-]prereqs \s*/\s* (assume-(?:provided|used))
          )$!ix}
                         $cfg->list_sections) {
        for my $param ($cfg->list_keys($section)) {
            my $v         = $cfg->get_value($section, $param);
            my $dir = $cfg->get_directive_before_key($section, $param);
            my $dir_s = $dir ? join(" ", @$dir) : "";
            #$log->tracef("section=%s, v=%s, param=%s, directive=%s", $section, $param, $v, $dir_s);

            $mods_from_ini{$param}   = $v unless $section =~ /assume-provided/;
            $assume_provided{$param} = $v if     $section =~ /assume-provided/;
            $assume_used{$param}     = $v if     $section =~ /assume-used/ ||
                $dir_s =~ /^lint[_-]prereqs\s+assume-used\b/m;
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

    my $perlv; # min perl v to use in x.yyyzzz (numified)format
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

    my @errs;
    for my $mod (keys %mods_from_ini) {
        my $v = $mods_from_ini{$mod};
        next if $mod eq 'perl';
        $log->tracef("Checking mod from dist.ini: %s (%s)", $mod, $v);
        my $is_core = Module::CoreList::More->is_still_core($mod, $v, $perlv);
        if (!$args{core_prereqs} && $is_core) {
            push @errs, {
                module  => $mod,
                req_v   => $v,
                is_core => 1,
                error   => "Core in perl ($perlv to latest) but ".
                    "mentioned in dist.ini",
                remedy  => "Remove from dist.ini",
            };
        }
        my $scanv = $mods_from_scanned{$mod};
        if (defined($scanv) && $scanv != 0 && version_ne($v, $scanv)) {
            push @errs, {
                module  => $mod,
                req_v   => $v,
                is_core => $is_core,
                error   => "Version mismatch between dist.ini ($v) ".
                    "and from scanned_prereqs ($scanv)",
                remedy  => "Fix either the code or version in dist.ini",
            };
        }
        unless (defined($scanv) || exists($assume_used{$mod})) {
            push @errs, {
                module  => $mod,
                req_v   => $v,
                is_core => $is_core,
                error   => "Unused but listed in dist.ini",
                remedy  => "Remove from dist.ini",
            };
        }
    }

    {
        no strict 'refs';
        my %lumped_mods;
        for my $mod (keys %mods_from_ini) {
            next unless $mod =~ /::Lumped$/;
            my $mod_pm = $mod;
            $mod_pm =~ s!::!/!g;
            $mod_pm .= ".pm";
            require $mod_pm;
            my $lm = \@{"$mod\::LUMPED_MODULES"};
            for (@$lm) { $lumped_mods{$_} = $mod }
        }
        last unless %lumped_mods;
        $log->tracef("Checking lumped modules");
        for my $mod (keys %lumped_mods) {
            my $v = $mods_from_ini{$mod};
            my $is_core = Module::CoreList::More->is_still_core($mod, $v, $perlv);
            if (exists $mods_from_ini{$mod}) {
                push @errs, {
                    module  => $mod,
                    req_v   => $v,
                    is_core => $is_core,
                    error   => "Listed in dist.ini but already lumped in $lumped_mods{$mod}",
                    remedy  => "Remove one of $mod or $lumped_mods{$mod} from dist.ini",
                };
            }
        }
    }

    for my $mod (keys %mods_from_scanned) {
        next if $mod eq 'perl';
        my $v = $mods_from_scanned{$mod};
        $log->tracef("Checking mod from scanned: %s (%s)", $mod, $v);
        my $is_core = Module::CoreList::More->is_still_core($mod, $v, $perlv);
        next if exists $pkgs{$mod};
        unless (exists($mods_from_ini{$mod}) ||
                    exists($assume_provided{$mod}) ||
                        ($args{core_prereqs} ? 0 : $is_core)) {
            push @errs, {
                module  => $mod,
                req_v   => $v,
                is_core => $is_core,
                error   => "Used but not listed in dist.ini",
                remedy  => "Put '$mod=$v' in dist.ini",
            };
        }
    }

    @errs = sort {prereq_ala_perlancar($a->{module}, $b->{module})} @errs;

    my $resmeta = {
        "cmdline.exit_code" => @errs ? 500-300:0,
        "table.fields" => [qw/module req_v is_core error remedy/],
    };
    [200, @errs ? "Extraneous/missing dependencies" : "OK", \@errs, $resmeta];
}

1;
#ABSTRACT:

=head1 SYNOPSIS

 # Use via lint-prereqs CLI script

=cut
