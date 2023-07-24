#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::ReposBranch
#
# DESCRIPTION
#   This class contains methods for gathering information for a repository
#   branch. It currently supports Subversion repository and local user
#   directory.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::ReposBranch;

# Standard pragma
use warnings;
use strict;

# Standard modules
use Carp;
use File::Spec;
use File::Spec::Functions;
use File::Basename;
use File::Find;

# FCM component modules
use Fcm::Util;
use Fcm::Timer;

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $branch = Fcm::ReposBranch->new (
#     CONFIG  => $config,
#     PACKAGE => $package,
#     TAG     => $tag,
#     REPOS   => $repos,
#     VERSION => $version,
#     TYPE    => $type,
#   );
#
# DESCRIPTION
#   This method constructs a new instance of the Fcm::ReposBranch class.
#
# ARGUMENTS
#   CONFIG  - reference to a Fcm::Config instance
#   PACKAGE - package name of which this repository belongs
#   TAG     - "tag" name of this branch of the repository
#   REPOS   - repository branch root URL/path
#   VERSION - this version of the branch is used by the extract
#   TYPE    - repository type
# ------------------------------------------------------------------------------

sub new {
  my $this  = shift;
  my %args  = @_;
  my $class = ref $this || $this;

  my $self = {
    CONFIG  => (exists $args{CONFIG}  ? $args{CONFIG}  : &main::cfg),
    PACKAGE => (exists $args{PACKAGE} ? $args{PACKAGE} : undef),
    TAG     => (exists $args{TAG}     ? $args{TAG}     : undef),
    REPOS   => (exists $args{REPOS}   ? $args{REPOS}   : undef),
    VERSION => (exists $args{VERSION} ? $args{VERSION} : undef),
    TYPE    => (exists $args{TYPE}    ? $args{TYPE}    : undef),

    # Use this list of directories in this branch
    DIRS    => {},

    # Expand this list of directories in this branch
    EXPDIRS => {},
  };

  bless $self, $class;
  return $self;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config = $branch->config;
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
#   $package = $branch->package;
#   $branch->package ($package);
#
# DESCRIPTION
#   This method returns the package name of this repository branch. If an
#   argument is specified, the name is set to the value of the argument.
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
#   $tag = $branch->tag;
#   $branch->tag ($tag);
#
# DESCRIPTION
#   This method returns the tag name of this repository branch. If an
#   argument is specified, the name is set to the value of the argument.
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
#   $repos = $branch->repos;
#   $branch->repos ($repos);
#
# DESCRIPTION
#   This method returns the URL/path name of this repository branch. If an
#   argument is specified, the URL/path is set to the value of the argument.
# ------------------------------------------------------------------------------

sub repos {
  my $self = shift;

  if (@_) {
    $self->{REPOS} = shift;
  }

  return $self->{REPOS};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $version= $branch->version;
#   $branch->version ($version);
#
# DESCRIPTION
#   This method returns the revision number of this repository branch. If an
#   argument is specified, the revision is set to the value of the argument.
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
#   $type = $branch->type;
#   $branch->type ($type);
#
# DESCRIPTION
#   This method returns the repository type ("svn" or "user"). If an
#   argument is specified, the type is set to the value of the argument.
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
#   $dir = $branch->dir ($name);
#   $branch->dir ($name, $dir);
#
# DESCRIPTION
#   This method returns the path to the source directory with sub-package name
#   $name. If $dir is specified, the path is set to its value.
# ------------------------------------------------------------------------------

sub dir {
  my $self = shift;
  my $name = shift;

  if (@_) {
    $self->{DIRS}{$name} = $_[0];
  }

  if (exists $self->{DIRS}{$name}) {
    return $self->{DIRS}{$name};

  } else {
    return undef;
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   %dirs = $branch->dirs;
#
# DESCRIPTION
#   This method returns a hash containing the source directories of this
#   repository. The keys of the hash are the sub-package names, and the values
#   of the hash are the URL/path to the source directories.
# ------------------------------------------------------------------------------

sub dirs {
  my $self = shift;

  return %{ $self->{DIRS} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $dir = $branch->expdir ($name);
#   $branch->expdir ($name, $dir);
#
# DESCRIPTION
#   This method returns the path to the expandable source directory with
#   sub-package name $name. If $dir is specified, the path is set to its
#   value.
# ------------------------------------------------------------------------------

sub expdir {
  my $self = shift;
  my $name = shift;

  if (@_) {
    $self->{EXPDIRS}{$name} = $_[0];
  }

  if (exists $self->{EXPDIRS}{$name}) {
    return $self->{EXPDIRS}{$name};

  } else {
    return undef;
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   %dirs = $branch->expdirs;
#
# DESCRIPTION
#   This method returns a hash containing the expandable source directories of
#   this repository. The keys of the hash are the sub-package names, and the
#   values of the hash are the URL/path to the expandable source directories.
# ------------------------------------------------------------------------------

sub expdirs {
  my $self = shift;

  return %{ $self->{EXPDIRS} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $branch->expand_version_tag;
#
# DESCRIPTION
#   This method expands the VERSION of the current repository branch to a
#   revision number.
# ------------------------------------------------------------------------------

sub expand_version_tag {
  my $self = shift;

  if ($self->type eq 'svn') {
    # Expand revision keyword
    my $rev = expand_rev_keyword (
      REV  => $self->version,
      URL  => $self->repos,
      HEAD => 1,
    );

    # Find out whether the specified revision is current or not
    if (uc ($self->version) ne 'HEAD' and $self->config->verbose > 1) {
      # Get last commit revision
      my $lc_rev = expand_rev_keyword (
        REV => 'HEAD',
        URL => $self->repos,
      );

      # Print information if used rev is less than the last commit rev
      print 'Info: using rev ', $rev, ' of ', $self->repos,
            ', last commit rev is ', $lc_rev, ".\n"
        if $rev < $lc_rev;
    }

    $self->version ($rev) if $rev ne $self->version;

  } elsif ($self->type eq 'user') {
    return;

  } else {
    e_report $self->repos, ': repository type "', $self->type,
             '" not supported.';
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $branch->expand_path;
#
# DESCRIPTION
#   This method expands the relative path names of sub-directories to full
#   path names. It returns 1 on success.
# ------------------------------------------------------------------------------

sub expand_path {
  my $self = shift;

  my $separator = $self->{CONFIG}->setting (qw/MISC DIR_SEPARATOR/);

  if ($self->type eq 'svn') {
    # SVN repository
    # Do nothing unless there is a declared repository for this branch
    return unless $self->{REPOS};

    # Remove trailing /
    $self->{REPOS} =~ s/$separator+$//;

    # Consider all declared (expandable) sub-directories
    for my $name (qw/DIRS EXPDIRS/) {
      for my $dir (keys %{ $self->{$name} }) {
        # Do nothing if declared sub-directory is quoted as a full URL
        next if &is_url ($self->{$name}{$dir});

        # Expand sub-directory to full URL
        $self->{$name}{$dir} = $self->{REPOS} . (
          $self->{$name}{$dir} ? ($separator . $self->{$name}{$dir}) : ''
        );
      }
    }
    # Note: "catfile" cannot be used in the above statement because it has
    #       the tendency of removing a slash from double slashes.

  } elsif ($self->type eq 'user') {
    # Local user directories

    # Expand leading ~ for all declared (expandable) sub-directories
    for my $name (qw/DIRS EXPDIRS/) {
      for my $dir (keys %{ $self->{$name} }) {
        $self->{$name}{$dir} = expand_tilde $self->{$name}{$dir};
      }
    }

    # A top directory for the source is declared
    if ($self->{REPOS}) {
      # Expand leading ~ for the top directory
      $self->{REPOS} = expand_tilde $self->{REPOS};

      # Get the root directory of the file system
      my $rootdir = File::Spec->rootdir ();

      # Expand top directory to absolute path, if necessary
      $self->{REPOS} = File::Spec->rel2abs ($self->{REPOS})
        if $self->{REPOS} !~ m/^$rootdir/;

      # Remove trailing /
      $self->{REPOS} =~ s/$separator+$//;

      # Consider all declared (expandable) sub-directories
      for my $name (qw/DIRS EXPDIRS/) {
        for my $dir (keys %{ $self->{$name} }) {
          # Do nothing if declared sub-directory is quoted as a full path
          next if $self->{$name}{$dir} =~ m#^$rootdir#;

          # Expand sub-directory to full path
          $self->{$name}{$dir} = $self->{$name}{$dir}
                               ? catfile ($self->{REPOS}, $self->{$name}{$dir})
                               : $self->{REPOS};
        }
      }
    }

  } else {
    e_report $self->repos, ': repository type "', $self->type,
             '" not supported.';
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $branch->expand_all;
#
# DESCRIPTION
#   This method searches the expandable source directories recursively for
#   source directories containing regular files. These sub-directories are
#   then added to the source directory list. The method returns total number
#   of sub-directories to be extracted from the current repository.
# ------------------------------------------------------------------------------

sub expand_all {
  my $self = shift;

  my %dirs = ();

  # Directory separator for SVN repository
  my $separator = $self->{CONFIG}->setting (qw/MISC DIR_SEPARATOR/);

  if ($self->type eq 'user') {
    for my $rootname (keys %{ $self->{EXPDIRS} }) {
      my %subdirs = find_srcdir $self->{EXPDIRS}{$rootname}, $rootname, '::';

      for my $key (keys %subdirs) {
        $dirs{$key} = $subdirs{$key};
      }
    }

  } elsif  ($self->type eq 'svn') {
    for my $rootname (keys %{ $self->{EXPDIRS} }) {
      # Execute the "svn ls -R" command
      my @lines   = &run_command (
        [qw/svn ls -R/, $self->{EXPDIRS}{$rootname} . '@' . $self->{VERSION}],
        METHOD => 'qx', TIME => $self->config->verbose > 2,
      );

      # Get list of sub-directories containing source files
      for my $line (@lines) {
        chomp $line;
        next if $line =~ /$separator$/;

        my $dir = dirname $line;

        my @pck = split /::/, $rootname;
        push @pck, (File::Spec->splitdir ($dir)) unless $dir eq '.';
        my $pck = join '::', @pck;

        my $val = $self->{EXPDIRS}{$rootname};
        $val   .= $separator . $dir unless $dir eq '.';

        $dirs{$pck} = $val;
      }
    }

  } else {
    e_report $self->repos, ': repository type "', $self->type,
             '" not supported.';
  }

  for my $key (keys %dirs) {
    $self->{DIRS}{$key} = $dirs{$key};
  }

  return scalar keys %dirs;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $branch->add_base_dirs ($base);
#
# DESCRIPTION
#   Add a list of source directories to the current branch based on the set
#   provided by $base, which must be a reference to a Fcm::ReposBranch
#   instance. It returns the total number of used sub-directories in the
#   current repositories.
# ------------------------------------------------------------------------------

sub add_base_dirs {
  my $self = shift;
  my $base = shift;

  my %base_dirs = $base->dirs;

  for my $key (keys %base_dirs) {
    # Remove repository root from base directories
    if ($base_dirs{$key} eq $base->repos) {
      $base_dirs{$key} = '';

    } else {
      $base_dirs{$key} = substr $base_dirs{$key}, length ($base->repos) + 1;
    }

    # Append base directories to current repository root
    $self->dir ($key, $base_dirs{$key}); 
  }

  # Expand relative path names of sub-directories
  $self->expand_path;

  return scalar keys %{ $self->{DIRS} };
  
}

# ------------------------------------------------------------------------------

1;

__END__
