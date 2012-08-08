package App::LintPrereqs;

use 5.010;
use strict;
use warnings;
use Log::Any qw($log);

use Config::IniFiles;
use File::Find;
use File::Which;
use Sort::Versions;

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
modules. Will complain if your prereqs is not actually used, or already in Perl
core. Will also complain if there are missing prereqs.

Designed to work with prereqs that are manually written. Does not work if you
use AutoPrereqs.

Configuration:

* [Extras / lint-prereqs / assume-used]

These are prerequisites that you know are used but can't be detected by
scan_prereqs. Or prerequisites that you want to include anyway.

* [Extras / lint-prereqs / assume-provided]

These can be used to list prerequisites that are detected by scan_prereqs, but
you know are already provided by some other modules. So to make lint-prereqs
pass, include them here.

_
    args => {
        default_perl_version => {
            schema => [str => {default=>'5.010000'}],
            summary => 'Perl version to use when unspecified',
        },
    },
};
sub lint_prereqs {
    my %args = @_;

    (-f "dist.ini")
        or return [412, "No dist.ini found, ".
                       "is your dist managed by Dist::Zilla?"];

    my $cfg = Config::IniFiles->new(-file => "dist.ini", -fallback => "ALL");
    $cfg or return [
        500, "Can't open dist.ini: ".join(", ", @Config::IniFiles::errors)];

    my %mods_from_ini;
    for my $section (grep {
        m!^(prereqs|extras \s*/\s* lint[_-]prereqs \s*/\s* assume-provided)!ix}
                         $cfg->Sections) {
        for my $param ($cfg->Parameters($section)) {
            my $v = $cfg->val($section, $param);
            #$log->tracef("section=$section, param=$param, v=$v");
            $mods_from_ini{$param} = $v;
        }
    }
    $log->tracef("mods_from_ini: %s", \%mods_from_ini);

    # this module is required, says dist.ini, and that's it. even when prereq
    # scanner says it's not used. examples are for forcing deps to spec dists
    # like Rinci or Setup.
    my %extra_mods_from_ini;
    for my $section (grep {
        m!^extras \s*/\s* lint[_-]prereqs \s*/\s* assume-used!ix}
                         $cfg->Sections) {
        for my $param ($cfg->Parameters($section)) {
            my $v = $cfg->val($section, $param);
            #$log->tracef("section=$section, param=$param, v=$v");
            $extra_mods_from_ini{$param} = $v;
        }
    }
    $log->tracef("extra_mods_from_ini: %s", \%extra_mods_from_ini);

    # assume package names from filenames, should be better and scan using PPI
    my %pkgs;
    find({
        #no_chdir => 1,
        wanted => sub {
            return unless /\.pm$/;
            my $pkg = $File::Find::dir;
            $pkg =~ s!^lib/!!;
            $pkg =~ s!/!::!g;
            $pkg .= "::$_";
            $pkg =~ s/\.pm$//;
            $pkgs{$pkg}++;
        },
    }, "lib");
    $log->tracef("Packages: %s", \%pkgs);

    my %mods_from_scanned;
    my $sppath = which("scan_prereqs")
        or return [412, "Can't find scan_prereqs in PATH"];
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

    my $perlv = $mods_from_ini{perl} // $mods_from_scanned{perl} // '5.010000';

    my %core_mods;
    my $clpath = which("corelist")
        or return [412, "Can't find corelist in PATH"];
    for (`$clpath -v $perlv`) {
        chomp;
        /^([\w:]+)(?:\s+(\S+))?\s*$/ or next;
        #do {
        #    warn "Invalid line from $clpath: $_, skipped";
        #    next;
        #};
        $core_mods{$1} = $2 // 0;
    }

    my $err;

    for my $mod (keys %mods_from_ini) {
        next if $mod eq 'perl';
        $log->tracef("Checking mod from dist.ini: %s", $mod);
        if (exists($core_mods{$mod}) &&
                versioncmp($core_mods{$mod}, $mods_from_ini{$mod}) >= 0) {
            $log->warnf("Module is core, but mentioned in dist.ini: %s", $mod);
            $err++;
        }
        unless (exists($mods_from_scanned{$mod}) ||
                    exists($extra_mods_from_ini{$mod})) {
            $log->warnf("Module doesn't seem to be used, ".
                            "but mentioned in dist.ini: %s", $mod);
            $err++;
        }
    }

    for my $mod (keys %mods_from_scanned) {
        next if $mod eq 'perl';
        $log->tracef("Checking mod from scanned: %s", $mod);
        next if exists $core_mods{$mod}; # XXX check version
        next if exists $pkgs{$mod};
        unless (exists $mods_from_ini{$mod}) {
            $log->errorf("Module is used, but not mentioned in dist.ini: %s",
                         $mod);
            $err++;
        }
    }

    $err ?
        [500, "Extraneous/missing dependencies", undef,
         {"cmdline.display_result"=>0}] :
            [200, "OK"];
}

1;
#ABSTRACT: Check extraneous/missing prerequisites in dist.ini

=head1 SYNOPSIS

 # Use via lint-prereqs CLI script

=cut
