#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   fcm_update_version_dir.pl
#
# SYNOPSIS
#   fcm_update_version_dir.pl [OPTIONS] [CFGFILE]
#
# DESCRIPTION
#   See $usage for detail.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

# Standard pragmas
use strict;
use warnings;

# Standard modules
use Getopt::Long;
use Cwd;
use File::Basename;
use File::Path;
use File::Spec;

# FCM component modules:
use lib File::Spec->catfile (dirname (dirname ($0)), 'lib');
use Fcm::Config;
use Fcm::Util;

# Usage
# ------------------------------------------------------------------------------
my $this  = basename $0;
my $usage = <<EOF;
NAME
  $this

SYNOPSIS
  $this [OPTIONS] [CFGFILE]

DESCRIPTION
  Update the version directories for a list of relative paths in the source
  repository URL.

OPTIONS
  -d [--dest] arg: Specify a destination for the extraction. If not specified,
                   the command extracts to the current working directory.
  -h [--help]    : Print help and exit.
  -f [--full]    : Specify the full mode. If not specified, the command runs
                   in incremental mode.
  -u [--url]  arg: Specify the source repository URL. No default.

ARGUMENTS
  A configuration file may be given to this command, or it will attempt to
  read from the standard input. Each line in the configuration must contain
  a relative path that resides under the given source repository URL. (Empty
  lines and lines beginning with a "#" are ignored.) Optionally, each
  relative path may be followed by a list of space separated "conditions".
  Each condition is a conditional operator (>, >=, <, <=, == or !=) followed
  by a revision number or the keyword HEAD. The command uses the revision
  log to determine the revisions at which the relative path has been updated
  in the source repository URL. If these revisions also satisfy the
  "conditions" set by the user, they will be considered in the extraction.
  In full mode, everything is re-extracted. In incremental mode, the version
  directories are only updated if they do not already exist.

COPYRIGHT
  This program is part of the FCM system.
  (C) Crown copyright Met Office. All rights reserved.
  For further details please refer to the file COPYRIGHT.txt
  which you should have received as part of this distribution.
EOF

# Options
# ------------------------------------------------------------------------------
my ($dest, $full, $help, $url);
GetOptions (
  'dest|d=s' => \$dest,
  'full|f'   => \$full,
  'help'     => \$help,
  'url|u=s'  => \$url,
);

if ($help) {
  print $usage;
  exit;
}

$dest = cwd () unless $dest;

die 'An URL must be specified with the --url option, abort' unless $url;

# Arguments
# ------------------------------------------------------------------------------
if (@ARGV) {
  die 'Cannot read: ', $ARGV[0], ', abort' unless -f $ARGV[0] and -r $ARGV[0];
}

# Get configuration settings
# ------------------------------------------------------------------------------
my $config = Fcm::Config->new ();
$config->get_config ();

# Expand URL keyword
$url = &expand_url_keyword (URL => $url);

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $cfg = &main::cfg ();
#
# DESCRIPTION
#   Return the $config variable.
# ------------------------------------------------------------------------------

sub cfg {
  return $config;
}

# ------------------------------------------------------------------------------

MAIN: {
  my $date = localtime;
  print $this, ': started on ', $date, "\n";

  my %dirs;

  # Read input (file) for a list directories and update conditions
  while (<>) {
    chomp;

    # Ignore empty and comment lines
    next if /^\s*(?:#|$)/;

    # Each line must contain a relative path, and optionally a list of
    # space delimited conditions
    my @words = split /\s+/;
    my $dir   = shift @words;

    # Check that the conditions are valid
    my @conditions;
    for my $word (@words) {
      if ($word =~ /^([<>]=?|[!=]=)(.+)$/i) {
        # Condition must be a conditional operator followed by a revision
        my ($operator, $rev) = ($1, $2);
        $rev = &expand_rev_keyword (REV => $rev, URL => $url, HEAD => 1);
        push @conditions, $operator . $rev;

      } else {
        print STDERR 'Warning: ignore unknown syntax for update condition: ',
                     $word, "\n";
      }
    }

    # Add directory and its conditions to a hash
    if ($dir =~ s#/\*$##) { # Directory finishes with wildcard

      # Run "svn ls" in recursive mode
      my $dirurl  = join ('/', ($url, $dir));
      my @files   = &run_command ([qw/svn ls -R/, $dirurl], METHOD => 'qx');

      # Find directories containing regular files
      while (my $file = shift @files) {
	# Skip directories
	next if $file =~ m#/$#;

	# Get "dirname" of regular file and add to hash
	my $subdir = join ('/', ($dir, dirname ($file)));
	$dirs{$subdir} = \@conditions;
      }

    } else {
      $dirs{$dir} = \@conditions;
    }

  }

  # Update each directory, if required
  for my $dir (sort keys %dirs) {
    # Use "svn log" to determine the revisions that need to be updated
    my %allversions;
    {
      my $command = 'svn log -q ' . join ('/', ($url, $dir));
      my @log     = &run_command (
        [qw/svn log -q/, join ('/', ($url, $dir))], METHOD => 'qx',
      );
      @log        = grep /^r\d+/, @log;

      # Assign a sequential "version" number to each sub-directory
      my $version = scalar @log;
      for (@log) {
        m/^r(\d+)/;
        $allversions{$1} = 'v' . $version--;
      }
    }
    my %versions = %allversions;

    # Extract only revisions matching the conditions
    if (@{ $dirs{$dir} }) {
      my @conditions = @{ $dirs{$dir} };

      for my $condition (@conditions) {
        for my $rev (keys %versions) {
          delete $versions{$rev} unless eval ($rev . $condition);
        }
      }
    }

    # Destination directory
    my $dirpath = File::Spec->catfile ($dest, $dir);

    if (-d $dirpath) {
      if ($full or not keys %versions) {
        # Remove destination directory top, in full mode
        # or if there are no matching revisions
        &run_command ([qw/rm -rf/, $dirpath], PRINT => 1);

      } else {
        # Delete excluded revisions if they exist, in incremental mode
        if (opendir DIR, $dirpath) {
          while (my $rev = readdir 'DIR') {
            next unless $rev =~ /^\d+$/;

            if (not grep {$_ eq $rev} keys %versions) {
              my @command = (qw/rm -rf/, File::Spec->catfile ($dirpath, $rev));
              &run_command (\@command, PRINT => 1);

              # Remove "version" symlink
              my $verlink = File::Spec->catfile ($dirpath, $allversions{$rev});
              unlink $verlink if -l $verlink;
            }
          }
          closedir DIR;
        }
      }
    }

    # Create container directory of destination if it does not already exist
    if (keys %versions and not -d $dirpath) {
      print '-> mkdir -p ', $dirpath, "\n";
      my $rc = mkpath $dirpath;
      die 'mkdir -p ', $dirpath, ' failed' unless $rc;
    }

    # Update each version directory that needs updating
    for my $rev (keys %versions) {
      my $revpath = File::Spec->catfile ($dest, $dir, $rev);

      # Create version directory if it does not exist
      if (not -e $revpath) {
        # Use "svn export" to create the version directory
        my @command = (
          qw/svn export -q -r/,
          $rev,
          join ('/', ($url, $dir)),
          $revpath,
        );

        &run_command (\@command, PRINT => 1);

        # Run GenScr_GenRule on any component directories
        # This can be removed once old versions of the SCSUI are no longer used
	if ($dir =~ m#(?:^|/)components/#) {
          my @command = (
            "GenScr_GenRule",
            File::Spec->catfile ($revpath, join ('.', (basename ($dir), "rule")))
          );

          &run_command (\@command, PRINT => 1);
	}
      }

      # Create "version" symlink if necessary
      my $verlink = File::Spec->catfile ($dest, $dir, $versions{$rev});
      symlink $rev, $verlink unless -l $verlink;
    }

    # Symbolic link to the "latest" version directory
    my $headlink = File::Spec->catfile ($dest, $dir, 'latest');
    my $headrev  = 0;
    for my $rev (keys %versions) {
      $headrev = $rev if $rev > $headrev;
    }

    if (-l $headlink) {
      # Remove old symbolic link if there is no revision to update or if it
      # does not point to the correct version directory
      my $org = readlink $headlink;
      unlink $headlink if (! $headrev or $org ne $headrev);
    }

    # (Re-)create the "latest" symbolic link, if necessary
    symlink $headrev, $headlink if ($headrev and not -l $headlink);
  }

  $date = localtime;
  print $this, ': finished normally on ', $date, "\n";
}

__END__
