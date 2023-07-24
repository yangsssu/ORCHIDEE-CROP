#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::SrcDirLayer
#
# DESCRIPTION
#   This class contains methods to manipulate the extract of a source
#   directory from a branch of a (Subversion) repository.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::SrcDirLayer;

# Standard pragma
use warnings;
use strict;

# Standard modules
use Carp;
use File::Spec;
use File::Spec::Functions;
use File::Basename;
use File::Path;
use File::Compare;

# FCM component modules
use Fcm::Util;
use Fcm::Timer;

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $layer = Fcm::SrcDirLayer->new (
#     CONFIG    => $config,
#     NAME      => $dir,
#     PACKAGE   => $package,
#     TAG       => $tag,
#     LOCATION  => $loc,
#     REPOSROOT => $repos,
#     VERSION   => $ver,
#     TYPE      => $type,
#     COMMIT    => $com,
#     EXTRACTED => $ext,
#     CACHEDIR  => $cac,
#   );
#
# DESCRIPTION
#   This method constructs a new instance of the Fcm::SrcDirLayer class.
#
# ARGUMENTS
#   CONFIG    - reference to a Fcm::Config instance
#   NAME      - sub-package name of the source directory
#   PACKAGE   - top level package name of which the current repository belongs
#   TAG       - package/revision tag of the current repository branch
#   LOCATION  - location of the source directory in the branch
#   REPOSROOT - repository root URL
#   VERSION   - revision of the repository branch
#   TYPE      - type of the repository branch ("svn" or "user")
#   COMMIT    - revision at which the source directory was changed
#   EXTRACTED - is this branch already extracted?
#   CACHEDIR  - cache directory for this directory branch
# ------------------------------------------------------------------------------

sub new {
  my $this  = shift;
  my %args  = @_;
  my $class = ref $this || $this;

  my $self = {
    CONFIG    => (exists $args{CONFIG}    ? $args{CONFIG}    : &main::cfg),
    NAME      => (exists $args{NAME}      ? $args{NAME}      : undef),
    PACKAGE   => (exists $args{PACKAGE}   ? $args{PACKAGE}   : undef),
    TAG       => (exists $args{TAG}       ? $args{TAG}       : undef),
    LOCATION  => (exists $args{LOCATION}  ? $args{LOCATION}  : undef),
    REPOSROOT => (exists $args{REPOSROOT} ? $args{REPOSROOT} : undef),
    VERSION   => (exists $args{VERSION}   ? $args{VERSION}   : undef),
    TYPE      => (exists $args{TYPE}      ? $args{TYPE}      : undef),
    COMMIT    => (exists $args{COMMIT}    ? $args{COMMIT}    : undef),
    EXTRACTED => (exists $args{EXTRACTED} ? $args{EXTRACTED} : undef),
    CACHEDIR  => (exists $args{CACHEDIR}  ? $args{CACHEDIR}  : undef),

    # List of source files in this directory branch
    FILES     => [],
  };

  bless $self, $class;
  return $self;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config = $layer->config;
#
# DESCRIPTION
#   This method returns a reference to the Fcm::Config instance.
# ------------------------------------------------------------------------------

sub config {
  my $self = shift;

  return $self->{CONFIG};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $name = $layer->name;
#   $layer->name ($name);
#
# DESCRIPTION
#   This method returns the sub-package name of the current source directory.
#   If an argument is specified, the sub-package name is set to the value of
#   the argument.
# ------------------------------------------------------------------------------

sub name {
  my $self = shift;

  if (@_) {
    $self->{NAME} = shift;
  }

  return $self->{NAME};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $package = $layer->package;
#   $layer->package ($package);
#
# DESCRIPTION
#   This method returns the top level package name in which the current source
#   directory belongs. If an argument is specified, the package name is set to
#   the value of the argument.
# ------------------------------------------------------------------------------

sub package {
  my $self = shift;

  if (@_) {
    $self->{PACKAGE} = shift;
  }

  return $self->{PACKAGE};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $tag = $layer->tag;
#   $layer->tag ($tag);
#
# DESCRIPTION
#   This method returns the branch/revision tag of the current repository
#   branch. If an argument is specified, the tag is set to the value of the
#   argument.
# ------------------------------------------------------------------------------

sub tag {
  my $self = shift;

  if (@_) {
    $self->{TAG} = shift;
  }

  return $self->{TAG};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $location = $layer->location;
#   $layer->location ($location);
#
# DESCRIPTION
#   This method returns the URL/location of the source directory in the
#   branch. If an argument is specified, the location is set to the value of
#   the argument.
# ------------------------------------------------------------------------------

sub location {
  my $self = shift;

  if (@_) {
    $self->{LOCATION} = shift;
  }

  return $self->{LOCATION};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $reposroot = $layer->reposroot;
#   $layer->reposroot ($reposroot);
#
# DESCRIPTION
#   This method returns the URL/location of the repository root of this
#   branch. If an argument is specified, the location is set to the value of
#   the argument.
# ------------------------------------------------------------------------------

sub reposroot {
  my $self = shift;

  if (@_) {
    $self->{REPOSROOT} = shift;
  }

  return $self->{REPOSROOT};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $version = $layer->version;
#   $layer->version ($version);
#
# DESCRIPTION
#   This method returns the revision number of this branch. If an argument is
#   specified, the revision number is set to the value of the argument.
# ------------------------------------------------------------------------------

sub version {
  my $self = shift;

  if (@_) {
    $self->{VERSION} = shift;
  }

  return $self->{VERSION};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $type = $layer->type;
#   $layer->type ($type);
#
# DESCRIPTION
#   This method returns the repository type ("svn" or "user"). If an argument is
#   specified, the type is set to the value of the argument.
# ------------------------------------------------------------------------------

sub type {
  my $self = shift;

  if (@_) {
    $self->{TYPE} = shift;
  }

  return $self->{TYPE};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $version = $layer->commit;
#   $layer->commit ($version);
#
# DESCRIPTION
#   This method returns the last modified revision of the source directory in
#   the branch. If an argument is specified, this revision is set to the value
#   of the argument.
# ------------------------------------------------------------------------------

sub commit {
  my $self = shift;

  if (@_) {
    $self->{COMMIT} = shift;
  }

  return $self->{COMMIT};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $extracted = $layer->extracted;
#   $layer->extracted ($extracted);
#
# DESCRIPTION
#   This method returns the "extracted flag" of the source directory branch
#   If an argument is specified, the flag is set to the value of the argument.
# ------------------------------------------------------------------------------

sub extracted {
  my $self = shift;

  if (@_) {
    $self->{EXTRACTED} = shift;
  }

  return $self->{EXTRACTED};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $dir = $layer->cachedir;
#   $layer->cachedir ($dir);
#
# DESCRIPTION
#   This method returns the cache directory of the source directory branch
#   If an argument is specified, the cache directory is set to the value of
#   the argument.
# ------------------------------------------------------------------------------

sub cachedir {
  my $self = shift;

  if (@_) {
    my $dir = shift;
    $self->{CACHEDIR} = $dir;
  }

  return $self->{CACHEDIR};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $dir = $layer->localdir;
#
# DESCRIPTION
#   This method returns the user or cache directory for the current revision
#   of the repository branch.
# ------------------------------------------------------------------------------

sub localdir {
  my $self = shift;

  return $self->user ? $self->{LOCATION} : $self->{CACHEDIR};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @files = $layer->files;
#
# DESCRIPTION
#   This method returns a list of regular files in this directory branch.
#   This method should only be called after a successful operation of the
#   get_files method that will be described below.
# ------------------------------------------------------------------------------

sub files {
  my $self = shift;

  return @{ $self->{FILES} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $user = $layer->user;
#
# DESCRIPTION
#   This method returns the string "user" if the current source directory
#   branch is a local directory. Otherwise, it returns "undef".
# ------------------------------------------------------------------------------

sub user {
  my $self = shift;

  return $self->{TYPE} eq 'user' ? 'user' : undef;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $version = $layer->get_commit;
#
# DESCRIPTION
#   If the current repository type is "svn", this method attempts to obtain
#   the revision in which the branch is last committed. On a successful
#   operation, it returns this revision number. Otherwise, it returns
#   "undef".
# ------------------------------------------------------------------------------

sub get_commit {
  my $self = shift;

  if ($self->type eq 'svn') {
    # Execute the "svn info" command
    my @lines   = &run_command (
      [qw/svn info/, $self->{LOCATION} . '@' . $self->{VERSION}],
      METHOD => 'qx', TIME => $self->config->verbose > 2,
    );

    my $rev;
    for (@lines) {
      if (/^Last\s+Changed\s+Rev\s*:\s*(\d+)/i) {
        $rev = $1;
        last;
      }
    }

    # Commit version of this source directory
    $self->{COMMIT} = $rev;

    return $self->{COMMIT};

  } elsif ($self->type eq 'user') {
    return;

  } else {
    e_report 'Repository type "', $self->type, '" not supported.';
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = $layer->update_cache;
#
# DESCRIPTION
#   If the current repository type is "svn", this method attempts to extract
#   the current version source directory from the current branch from the
#   repository, sending the output to the cache directory. It returns true on
#   a successful operation, or false if the repository is not of type "svn".
# ------------------------------------------------------------------------------

sub update_cache {
  my $self = shift;

  return unless $self->{CACHEDIR};

  # Create cache extract destination, if necessary
  my $dirname = dirname $self->{CACHEDIR};
  mkpath $dirname if not -d $dirname;

  e_report $dirname, ': cannot write to cache, abort.'
    unless -d $dirname and -w $dirname;
  
  if ($self->type eq 'svn') {
    # Set up the extract command, "svn export --force -q -N"
    my @command = (
      qw/svn export --force -q -N/,
      $self->{LOCATION} . '@' . $self->{VERSION},
      $self->{CACHEDIR},
    );

    &run_command (\@command, TIME => $self->config->verbose > 2);

  } elsif ($self->type eq 'user') {
    return;

  } else {
    e_report 'Repository type "', $self->type, '" not supported.';
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @files = $layer->get_files;
#
# DESCRIPTION
#   This method returns a list of files in the cache or local user directory
#   of the current version of the source directory in the current branch.
# ------------------------------------------------------------------------------

sub get_files {
  my $self = shift;

  # Get a list of files in the cache (or local user) directory
  my @files = ();

  opendir (DIR, $self->localdir)
    or die $self->localdir, ': cannot read directory';

  while (my $file = readdir DIR) {
    next if $file =~ /^\.\.?/;                   # ignore . and .. and hidden
    next if $file =~ /~$/;                       # ignore emacs swap files
    next if -d catfile ($self->localdir, $file); # ignore sub-directories
    push @files, $file;
  }
  closedir DIR;

  # Return the (base name) of the list of files
  $self->{FILES} = \@files;
  return @files;
}

# ------------------------------------------------------------------------------

1;

__END__
