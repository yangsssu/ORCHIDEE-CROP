#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::Extract
#
# DESCRIPTION
#   This class contains methods for carrying out the various tasks that are
#   required to extract code from the FCM Subversion repository for feeding
#   into the prototype build system. At the end of the extract, it writes a
#   build configuration file for feeding into the build system.  If the code
#   is to be built on a remote machine, it is mirrored to the remote machine
#   using a "rdist" or "rsync" interface.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::Extract;

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
use Fcm::CfgFile;
use Fcm::ReposBranch;
use Fcm::SrcDirLayer;
use Fcm::Util;
use Fcm::Timer;

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ext = Fcm::Extract->new (
#     CONFIG    => $config,
#     CFG_SRC   => $cfg_src,
#     EXTRACTED => $extracted,
#   );
#
# DESCRIPTION
#   This method constructs a new instance of the Fcm::Extract class.
#
# ARGUMENTS
#   CONFIG     - reference to a Fcm::Config instance
#   CFG_SRC    - source path to the extract configuration file
#   EXTRACTED  - is it a pre-extracted object?
# ------------------------------------------------------------------------------

sub new {
  my $this  = shift;
  my %args  = @_;
  my $class = ref $this || $this;

  my $cfg       = exists $args{CFG_SRC}   ? $args{CFG_SRC}   : undef;
  my $extracted = exists $args{EXTRACTED} ? $args{EXTRACTED} : undef;
  my $config    = exists $args{CONFIG}    ? $args{CONFIG}    : &main::cfg;

  my $self = {
    CONFIG     => $config,            # configuration settings
    CFG        => Fcm::CfgFile->new ( # ext cfg for this extract
      SRC      => $cfg,               # source path of the config file
      TYPE     => 'ext',              # config file type
      CONFIG   => $config,            # configuration settings
    ),
    DEST       => {                   # destination info for this extract
      ROOTDIR  => undef,              # destination root directory
      CACHEDIR => undef,              # extract cache directory
      CFGDIR   => undef,              # destination configuration directory
      SRCDIR   => undef,              # destination source directory
      BLD_CFG  => undef,              # bld cfg for the build system
      EXT_CFG  => undef,              # ext cfg for subsequent extract
    },
    RDEST      => {                   # remote destination information
      MACHINE  => undef,              # destination machine
      LOGNAME  => undef,              # remote login name
      ROOTDIR  => undef,              # destination root directory
      CFGDIR   => undef,              # destination configuration directory
      SRCDIR   => undef,              # destination source directory
      BLD_CFG  => undef,              # bld cfg for the build system
      EXT_CFG  => undef,              # ext cfg for subsequent extract
    },
    BDECLARE   => [],                 # list of declared bld cfg entries
    OVERRIDE   => 0,                  # override conflicting patches?
    EXTRACTED  => $extracted,         # is the current object pre-extracted?
    USE        => [],                 # list of re-used extracts
    BRANCHES   => [],                 # list of repository branch info
    SRCDIRS    => {},                 # list of source directory extract info
    LOCK       => undef,              # lock file
  };
  bless $self, $class;
  return $self;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->DESTROY;
#
# DESCRIPTION
#   This method is called automatically when a Fcm::Extract object is
#   destroyed.
# ------------------------------------------------------------------------------

sub DESTROY {
  my $self = shift;

  # Remove the lock if it is set
  unlink $self->{LOCK} if $self->{LOCK} and -e $self->{LOCK};

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config = $ext->config;
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
#   $cfgfile = $ext->cfg;
#   $ext->cfg ($cfgfile);
#
# DESCRIPTION
#   This method returns a reference to a Fcm::CfgFile instance for the extract
#   configuration file.
# ------------------------------------------------------------------------------

sub cfg {
  my $self = shift;

  return $self->{CFG};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $dest = $ext->dest ([$name]);
#
# DESCRIPTION
#   This method returns a hash containing the extract destination information
#   (local) if no argument is specified. If $name is specified, it returns the
#   named hash element if it exists.
# ------------------------------------------------------------------------------

sub dest {
  my $self = shift;

  if (@_) {
    my $name = shift;
    $name    = uc $name;

    if (exists $self->{DEST}{$name}) {
      return $self->{DEST}{$name};
    }
  }

  return %{ $self->{DEST} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rdest = $ext->rdest ([$name]);
#
# DESCRIPTION
#   This method returns a hash containing the extract destination information
#   (remote) if no argument is specified. If $name is specified, it returns the
#   named hash element if it exists.
# ------------------------------------------------------------------------------

sub rdest {
  my $self = shift;

  if (@_) {
    my $name = shift;
    $name    = uc $name;

    if (exists $self->{RDEST}{$name}) {
      return $self->{RDEST}{$name};
    }
  }

  return %{ $self->{RDEST} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @bdeclare = $ext->bdeclare ();
#
# DESCRIPTION
#   This method returns a list containing the build configuration file entries.
# ------------------------------------------------------------------------------

sub bdeclare {
  my $self = shift;

  return @{ $self->{BDECLARE} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @branches = $ext->branches ([$index]);
#
# DESCRIPTION
#   This method returns a list of references to Fcm::ReposBranch instances. If
#   $index is specified, it returns the numbered item in the list.
# ------------------------------------------------------------------------------

sub branches {
  my $self = shift;

  if (@_) {
    my $index = $_[0];
    return exists $self->{BRANCHES}[$index] ? $self->{BRANCHES}[$index] : undef;
  }

  return @{ $self->{BRANCHES} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   %srcdirs = $ext->srcdirs ([$name]);
#
# DESCRIPTION
#   This method returns a hash of source directories to be processed by this
#   extract. If $name is specified, a named element of the hash is returned
#   instead.
# ------------------------------------------------------------------------------

sub srcdirs {
  my $self = shift;

  if (@_) {
    my $name = shift;
    $name    = uc $name;

    return exists $self->{SRCDIRS}{$name} ? $self->{SRCDIRS}{$name} : undef;
  }

  return %{ $self->{SRCDIRS} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = $ext->extract ([FULL => 1], [IGNORE_LOCK => 1]);
#
# DESCRIPTION
#   This is the main class method. It performs an extract using the current
#   configuration. If FULL is set to true, it runs in full mode. Otherwise, it
#   runs in incremental mode. If IGNORE_LOCK is set to true, it ignores any lock
#   files that may exist in the extract destination root directory.
# ------------------------------------------------------------------------------

sub extract {
  my $self = shift;
  my %args = @_;

  my $full        = exists $args{FULL}        ? $args{FULL}        : 0;
  my $ignore_lock = exists $args{IGNORE_LOCK} ? $args{IGNORE_LOCK} : 0;

  my $verbose = $self->config->verbose;

  my $date = localtime;
  print 'Extract started on ', $date, '.', "\n" if $verbose;
  my $otime = time;

  my $rc;
  $rc = $self->decipher_cfg;

  print '->Extract: start', "\n" if $verbose;
  my $stime = time;

  $rc = $self->check_dest               if $rc;
  $rc = $self->check_lock               if $rc and not $ignore_lock;
  $rc = $self->_set_lock                if $rc;
  $rc = $self->expand_cfg               if $rc;
  $rc = $self->_create_dest_dir ($full) if $rc;
  $rc = $self->create_dir_stack         if $rc;
  $rc = $self->_extract_src             if $rc;

  $rc = $self->_sort_bdeclare if $rc;
  $rc = $self->_write_ext_cfg if $rc;
  $rc = $self->_write_bld_cfg if $rc;

  my $ftime = time;
  my $s_str = $ftime - $stime > 1 ? 'seconds' : 'second';
  print '->Extract: ', $ftime - $stime, ' ', $s_str, "\n";

  if ($rc and $self->{RDEST}{MACHINE}) {
    print '->Mirror : start', "\n" if $verbose;
    $stime = time;
    $rc = $self->_mirror_extract;
    $ftime = time;
    $s_str = $ftime - $stime > 1 ? 'seconds' : 'second';
    print '->Mirror : ', $ftime - $stime, ' ', $s_str, "\n";
  }

  if ($verbose) {
    $s_str = $ftime - $otime > 1 ? 'seconds' : 'second';
    print '->TOTAL  : ', $ftime - $otime, ' ', $s_str, "\n";
  }

  $date = localtime;
  if ($rc) {
    print 'Extract command finished on ', $date, '.', "\n" if $verbose;

  } else {
    e_report 'Extract command failed on ', $date, '.';
  }

  return $rc;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ext->decipher_cfg ();
#
# DESCRIPTION
#   This method deciphers the extract configuration file.
# ------------------------------------------------------------------------------

sub decipher_cfg {
  my $self = shift;

  return unless $self->cfg->src;

  # Read config file
  my $read = $self->cfg->read_cfg;

  # Check config file type
  if ($read) {
    if ($self->cfg->type ne 'ext') {
      w_report 'Error: ', $self->cfg->src, ': not an extract config file';
      return;
    }

  } else {
    return;
  }

  my %cfg_labels = %{ $self->config->setting ('CFG_LABEL') };

  # Extract information from each line of the config file
  my @lines    = $self->cfg->lines;
  LINE: for my $line (@lines) {
    my $label = $line->{LABEL};
    my $value = $line->{VALUE};

    next LINE unless $label;

    # Configuration file type/version, ignore
    for my $my_label (keys %{ $cfg_labels{CFGFILE} }) {
      next LINE if uc ($label) eq uc ($cfg_labels{CFGFILE}{$my_label});
    }

    # Include another file, processed already, ignore this line
    next LINE if uc ($label) eq $cfg_labels{INC};

    # User variable, ignore
    next LINE if index (uc ($label), '%') == 0;

    # Local destination directories, config file, etc
    for my $my_label (keys %{ $cfg_labels{DEST} }) {
      if (uc ($label) eq uc ($cfg_labels{DEST}{$my_label})) {
        $self->{DEST}{$my_label} = &expand_tilde ($value);
        next LINE;
      }
    }

    # Remote machine, logname, destination directories, config file, etc
    for my $my_label (keys %{ $cfg_labels{RDEST} }) {
      if (uc ($label) eq uc ($cfg_labels{RDEST}{$my_label})) {
        $self->{RDEST}{$my_label} = $value;
        next LINE;
      }
    }

    # "USE" statements
    if (uc ($label) eq uc ($cfg_labels{USE})) {
      my $exists = grep {$_->cfg->src eq $value} @{ $self->{USE} };

      # Initialise new Fcm::Extract object if not already exists
      unless ($exists) {
        my $extract = Fcm::Extract->new (
          CONFIG    => $self->config,
          CFG_SRC   => expand_tilde ($value),
          EXTRACTED => 1,
        );

        $extract->decipher_cfg;
        $extract->check_dest;
        $extract->expand_cfg ();
        push @{ $self->{USE} }, $extract;
      }
      next LINE;
    }

    # "Override" setting
    if (uc ($label) eq uc ($cfg_labels{OVERRIDE})) {
      $self->{OVERRIDE} = $value;
      next LINE;
    }

    # "Mirror" command
    if (uc ($label) eq uc ($cfg_labels{MIRROR})) {
      $self->config->assign_setting (
        LABELS => [qw/TOOL MIRROR/],
        VALUE  => $value,
      );
      next LINE;
    }

    # Declared bld cfg entries
    {
      my $prefix = $cfg_labels{BDECLARE} . '::';

      if (index (uc ($label), $prefix) == 0) {
        my $name = substr $label, length ($prefix);

        if ($name) {
          push @{ $self->{BDECLARE} }, {LABEL => $name, VALUE => $value,};
          next LINE;
        }
      }
    }

    # Repository, version and source directories
    for my $my_label (qw/REPOS VERSION SRCDIR EXPSRCDIR/) {
      my $prefix  = $cfg_labels{$my_label} . '::';

      if (index (uc ($label), $prefix) == 0) {
        my $name    = substr $label, length ($prefix);

        # Detemine package and tag
        my @names   = split /::/, $name;
        my $tag     = pop @names;
        my $pckroot = $names[0];
        my $pck     = join '::', @names;

        # Check that $tag and $pckroot are defined
        last if not $tag;
        last if not $pckroot;

        # Check whether branch already exists
        my @branches = grep {
          $_->package eq $pckroot and $_->tag eq $tag
        } @{ $self->{BRANCHES} };

        my $branch   = undef;

        if (@branches) { # If so, set $branch to point to existing branch
          $branch = shift @branches;

        } else {         # If not, create new branch
          $branch = Fcm::ReposBranch->new (
            CONFIG  => $self->config,
            PACKAGE => $pckroot,
            TAG     => $tag,
          );

          push @{ $self->{BRANCHES} }, $branch;
        }

        # Check package name for source directory declarations
        if ($my_label eq 'SRCDIR' or $my_label eq 'EXPSRCDIR') {
          if ($pck eq $pckroot and $value !~ m#^/#) {
            # Sub-package name not set and source directory quoted as a relative
            # path, determine package name from path name
            my @subpck = File::Spec->splitdir ($value);
            $pck       = join '::', ($pckroot, @subpck);
          }
        }

        # Assign the value accordingly
        if ($my_label eq 'REPOS') {          # Repository location
          $branch->repos ($value);

        } elsif ($my_label eq 'VERSION') {   # Version used
          $branch->version ($value);

        } elsif ($my_label eq 'SRCDIR') {    # Source directory used
          $branch->dir ($pck, $value);

        } elsif ($my_label eq 'EXPSRCDIR') { # Expandable source directory
          $branch->expdir ($pck, $value);
        }

        next LINE;
      }
    }

    # Label not recognised
    w_report 'ERROR: ', $line->{SRC}, ': LINE ', $line->{NUMBER},
             ': label "', $label, '" not recognised';
    return;
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ext->check_dest ();
#
# DESCRIPTION
#   This method checks that the extract destionations are set correctly.
# ------------------------------------------------------------------------------

sub check_dest {
  my $self = shift;

  my %subdir  = %{ $self->config->setting ('DIR') };
  my %cfgname = %{ $self->config->setting ('CFG_NAME') };

  # Default destination settings
  my $dest = $self->{DEST};
  if ($dest->{ROOTDIR}) {
    unless ($dest->{SRCDIR}) {   # Location of extracted source
      $dest->{SRCDIR} = catfile $dest->{ROOTDIR}, $subdir{SRC};
    }
    unless ($dest->{CFGDIR}) {   # Location of configuration files
      $dest->{CFGDIR} = catfile $dest->{ROOTDIR}, $subdir{CFG};
    }
    unless ($dest->{CACHEDIR}) { # Location of cache
      $dest->{CACHEDIR} = catfile $dest->{ROOTDIR}, $subdir{CACHE};
    }
    unless ($dest->{BLD_CFG}) {  # Location of (output) bld cfg
      $dest->{BLD_CFG} = catfile $dest->{CFGDIR}, $cfgname{BLD};
    }
    unless ($dest->{EXT_CFG}) {  # Location of (output) ext cfg
      $dest->{EXT_CFG} = catfile $dest->{CFGDIR}, $cfgname{EXT};
    }
  } else {
    w_report 'Error: ', $self->cfg->src,
             ': destination root directory not set.';
    return;
  }

  # Default remote destination settings
  if ($self->{RDEST}{MACHINE}) {

    # Use local logname as remote logname if it is not set
    $self->{RDEST}{LOGNAME} = getlogin      unless $self->{RDEST}{LOGNAME};
    $self->{RDEST}{LOGNAME} = $ENV{LOGNAME} unless $self->{RDEST}{LOGNAME};
    $self->{RDEST}{LOGNAME} = $ENV{USER}    unless $self->{RDEST}{LOGNAME};

    unless ($self->{RDEST}{LOGNAME}) {
      w_report 'Error: ', $self->cfg->src,
               ': cannot determine your remote logname.';
      return;
    }

    # Make sure remote destination root directory is set
    unless ($self->{RDEST}{ROOTDIR}) {
      w_report 'Error: ', $self->cfg->src,
               ': remote destination root directory not set.';
      return;
    }

    # Make sure remote destination source directory is set
    $self->{RDEST}{SRCDIR} = catfile $self->{RDEST}{ROOTDIR}, $subdir{SRC}
      unless $self->{RDEST}{SRCDIR};

    # Make sure remote destination configuration directory is set
    $self->{RDEST}{CFGDIR} = catfile $self->{RDEST}{ROOTDIR}, $subdir{CFG}
      unless $self->{RDEST}{CFGDIR};

    # Make sure remote bld cfg is set
    $self->{RDEST}{BLD_CFG} = catfile $self->{RDEST}{CFGDIR}, $cfgname{BLD}
      unless $self->{RDEST}{BLD_CFG};

    # Make sure remote ext cfg is set
    $self->{RDEST}{EXT_CFG} = catfile $self->{RDEST}{CFGDIR}, $cfgname{EXT}
      unless $self->{RDEST}{EXT_CFG};

  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ext->check_lock ();
#
# DESCRIPTION
#   This method checks whether a lock is set in the current extract.
# ------------------------------------------------------------------------------

sub check_lock {
  my $self = shift;

  my $rootdir  = $self->{DEST}{ROOTDIR};
  my $lock_ext = catfile ($rootdir, $self->config->setting (qw/MISC LOCK_EXT/));
  my $lock_bld = catfile ($rootdir, $self->config->setting (qw/MISC LOCK_BLD/));

  # Always throw error if extract lock exists
  if (-e $lock_ext) {
    w_report 'ERROR: extract lock file exists: ', $lock_ext, ',';
    w_report '       an extract may be running at ', $rootdir, ', abort.';
    return;
  }

  # Throw error if current object is not a "used" pre-extracted object and
  # a build lock exists
  if ((not $self->{EXTRACTED}) and -e $lock_bld) {
    w_report 'ERROR: build lock file exists: ', $lock_bld, ',';
    w_report '       a build may be running at ', $rootdir, ', abort.';
    return;
  }

  # Check locks in inherited extract
  for my $use (@{ $self->{USE} }) {
    return unless $use->check_lock;
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_set_lock ();
#
# DESCRIPTION
#   This method sets a lock is set in the current extract.
# ------------------------------------------------------------------------------

sub _set_lock {
  my $self = shift;

  $self->{LOCK} = catfile (
    $self->{DEST}{ROOTDIR}, $self->config->setting (qw/MISC LOCK_EXT/),
  );

  &touch_file ($self->{LOCK});

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ext->expand_cfg ();
#
# DESCRIPTION
#   This method expands the settings of the extract configuration.
# ------------------------------------------------------------------------------

sub expand_cfg {
  my $self = shift;

  # Establish a set of source directories from the "base repository"
  my %base_branches = ();

  # Inherit "base" set of source directories from re-used extracts
  my @uses = @{ $self->{USE} };

  for my $use (@uses) {
    my @branches = $use->branches;

    for my $branch (@branches) {
      my $package              = $branch->package;
      $base_branches{$package} = $branch unless exists $base_branches{$package};
    }
  }

  for my $branch (@{ $self->{BRANCHES} }) {
    # Expand URL keywords if necessary
    if ($branch->repos) {
      my $repos = expand_url_keyword (
        URL => $branch->repos,
        CFG => $self->config,
      );
      $branch->repos ($repos) if $repos ne $branch->repos;
    }

    # Check that repository type and version are set
    if ($branch->repos and &is_url ($branch->repos)) {
      $branch->type    ('svn')  unless $branch->type;
      $branch->version ('head') unless $branch->version;

    } else {
      $branch->type    ('user') unless $branch->type;
      $branch->version ('user') unless $branch->version;
    }

    $branch->expand_version_tag; # Work out revision number a version tag
    $branch->expand_path;        # Expand relative path to full path
    $branch->expand_all;         # Search sub-directories

    my $package = $branch->package;

    if (exists $base_branches{$package}) {
      # A base branch for this package exists

      # If current branch has no source directory, use the set provided by the
      # base branch
      my %dirs = $branch->dirs;
      $branch->add_base_dirs ($base_branches{$package}) unless keys %dirs;

    } else {
      # This package does not yet have a base branch, set this branch as base
      $base_branches{$package} = $branch;
    }
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_create_dest_dir ($full);
#
# DESCRIPTION
#   This internal method (re-)creates all the destination directories if
#   necessary. If $full is set to true, it removes existing directories/files
#   in the destination directories.
# ------------------------------------------------------------------------------

sub _create_dest_dir {
  my ($self, $full) = @_;

  my $verbose = $self->config->verbose;

  # Remove previous extract if "FULL" flag is set
  if ($full) {
    # Remove extracted source
    if (-d $self->{DEST}{SRCDIR} and -w $self->{DEST}{SRCDIR}) {
      print 'Remove directory: ', $self->{DEST}{SRCDIR}, "\n" if $verbose;
      my $removed = rmtree $self->{DEST}{SRCDIR};
      w_report 'WARNING: ', $self->{DEST}{SRCDIR}, ': cannot remove.'
        if not $removed;
    }

    # Remove cache
    my @files;
    if (-d $self->{DEST}{CACHEDIR} and opendir DIR, $self->{DEST}{CACHEDIR}) {
      @files = grep {$_ ne '.' and $_ ne '..'} readdir 'DIR';
      closedir DIR;
    }

    for my $file (@files) {
      my $path = File::Spec->catfile ($self->{DEST}{CACHEDIR}, $file);

      next unless $file eq $self->config->setting (qw/CACHE EXTCONFIG/) or
                  -d $path;

      print 'Remove: ', $path, "\n" if $verbose;
      my $removed = rmtree $path;
      w_report 'WARNING: ', $path, ': cannot remove.' if not $removed;
    }
  }

  # Create extract destinations if necessary
  for my $my_label (qw/ROOTDIR CACHEDIR CFGDIR SRCDIR/) {
    my $dirname = $self->{DEST}{$my_label};

    # Create directory if it does not already exist
    if (not -d $dirname) {
      print 'Make directory: ', $dirname, "\n" if $verbose > 1;
      mkpath $dirname;
    }

    unless (-d $dirname and -w $dirname) {
      w_report 'ERROR: ', $dirname, ': cannot write to destination.';
      return;
    }
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ext->create_dir_stack (
#     USE => $use, # Is this a pre-extracted configuration?
#   );
#
# DESCRIPTION
#   This method creates a hash of source directories to be processed. If the
#   flag USE is set to true, the source directories are assumed processed and
#   extracted.
# ------------------------------------------------------------------------------

sub create_dir_stack {
  my $self = shift;
  my %args = @_;
  my $extracted = exists $args{USE} ? $args{USE} : undef;

  # Inherit from USE ext cfg
  if (@{ $self->{USE} } > 0) {
    for my $use (@{ $self->{USE} }) {
      $use->create_dir_stack (USE => 1);
      my %use_srcdirs = $use->srcdirs;

      while (my ($key, $value) = each %use_srcdirs) {
        $self->{SRCDIRS}{$key} = $value;

        # Re-set destination to current destination
        my @path = split (/::/, $key);
        $self->{SRCDIRS}{$key}{DEST} = catfile ($self->{DEST}{SRCDIR}, @path);
      }
    }
  }

  # Build stack from current ext cfg
  for my $branch (@{ $self->{BRANCHES} }) {
    my %branch_dirs = $branch->dirs;

    for my $dir (keys %branch_dirs) {
      # Check whether source directory is already in the list
      if (not exists $self->{SRCDIRS}{$dir}) { # if not, create it
        $self->{SRCDIRS}{$dir} = {
          DEST  => catfile ($self->{DEST}{SRCDIR}, split (/::/, $dir)),
          STACK => [],
          FILES => {},
        };
      }

      my $stack = $self->{SRCDIRS}{$dir}{STACK}; # copy reference

      # Create a new layer in the input stack
      my $layer = Fcm::SrcDirLayer->new (
        CONFIG    => $self->config,
        NAME      => $dir,
        PACKAGE   => $branch->package,
        TAG       => $branch->tag,
        LOCATION  => $branch->dir ($dir),
        REPOSROOT => $branch->repos,
        VERSION   => $branch->version,
        TYPE      => $branch->type,
        EXTRACTED => $extracted ? $self->{SRCDIRS}{$dir}{DEST} : undef,
      );

      # Check whether layer is already in the stack
      my $exist = grep {
        $_->location eq $layer->location and $_->version  eq $layer->version;
      } @{ $stack };

      if (not $exist) {
        # If not already exist, put layer into stack

        # Note: user stack always comes last
        if (! $layer->user and exists $stack->[-1] and $stack->[-1]->user) {
          my $lastlayer = pop @{ $stack };
          push @{ $stack }, $layer;
          $layer = $lastlayer;
        }

        push @{ $stack }, $layer;

      } elsif ($layer->user) {

        # User layer already exists, overwrite it
        $stack->[-1] = $layer;

      }
    }
  }

  # Read content of "commit cache" file if it exists
  my $cachedir = $self->{DEST}{CACHEDIR};
  my $cfgbase  = $self->config->setting (qw/CACHE EXTCONFIG/);
  my $cfgfile  = catfile $cachedir, $cfgbase;
  my %config_lines = ();
  if (-r $cfgfile) {
    my $cfg = Fcm::CfgFile->new (CONFIG => $self->config, SRC => $cfgfile,);
    $cfg->read_cfg;
    my @lines = $cfg->lines;

    for my $line (@lines) {
      $config_lines{$line->{LABEL}} = $line->{VALUE};
    }
  }

  my %new_config_lines;

  # Compare each layer to base layer, discard unnecessary layers
  for my $srcdir (keys %{ $self->{SRCDIRS} }) {
    my @stack = ();

    while (my $layer = shift @{ $self->{SRCDIRS}{$srcdir}{STACK} }) {
      if ($layer->user) {
        # User directory, check that the declared location exists
        if (not -d $layer->location) {
          w_report 'Error: declared source directory ', $layer->location,
                   ' does not exists ';
          return;
        }

        # Always override repository code
        push @stack, $layer;

      } else {
        unless ($layer->extracted and $layer->commit) {

          my $key = join '::', ($srcdir, $layer->location, $layer->version);

          # See if commit version information is cached
          if (keys %config_lines) {
            if (exists $config_lines{$key}) {
              $layer->commit ($config_lines{$key});
            }
          }

          # Check source directory for commit version, if necessary
          $layer->get_commit unless $layer->commit;
          if (not $layer->commit) {
            w_report 'Error: cannot determine the last changed revision of ',
                     $layer->location;
            return;
          }

          # Set cache directory for layer
          my $tag_ver = $layer->tag . '__' . $layer->commit;
          $layer->cachedir (catfile $cachedir, split (/::/, $srcdir), $tag_ver);

          # New line in cache config file
          $new_config_lines{$key} = $layer->commit;
        }

        # Push this layer in the stack:
        # 1. it has a different version compared to the top layer
        # 2. it is the top layer (base line code)
        if (@stack > 0) {
          push @stack, $layer if $layer->commit != $stack[0]->commit;

        } else {
          push @stack, $layer;
        }

      }
    }

    $self->{SRCDIRS}{$srcdir}{STACK} = \@stack;

  }

  # Write "commit cache" file
  if (not $extracted) {
    mkpath $cachedir if not -d $cachedir;
    my $cfg = Fcm::CfgFile->new (CONFIG => $self->config,);

    while ((my $label, my $value) = each %new_config_lines) {
      $cfg->add_line (LABEL => $label, VALUE => $value,);
    }

    $cfg->print_cfg ($cfgfile);
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_extract_src ();
#
# DESCRIPTION
#   This internal method performs the extract of the source directories and
#   files if necessary.
# ------------------------------------------------------------------------------

sub _extract_src {
  my $self = shift;

  my $verbose = $self->config->verbose;
  my %v_count = (
    CREATED_DIRS    => 0,
    IGNORED_SUBDIRS => 0,
    UPDATED_FILES   => 0,
    REMOVED_FILES   => 0,
  );

  my $cachedir = $self->{DEST}{CACHEDIR};

  # Go through the "stack" of each source directory
  # Extract the source directories/files if required

  for my $srcdir (values %{ $self->{SRCDIRS} }) {

    # Check if destionation exists and is not a directory
    if (-f $srcdir->{DEST}) {
      w_report $srcdir->{DEST},
               ': destination exists and is not a directory, abort.';
      return;
    }

    my %base_files   = (); # list of files in the base layer
    my %used_files   = (); # keys = file basenames, values = layer reference
    $srcdir->{FILES} = \%used_files;
    my @destpath     = (); # search path for source directory destinations
 
    for my $layer (@{ $srcdir->{STACK} }) {
      # Update the cache for each layer of the stack if necessary
      $layer->update_cache unless $layer->extracted or -d $layer->localdir;

      # Search path for extract destinations of this source directory
      unshift @destpath, $layer->extracted
        if $layer->extracted and not grep {$_ eq $layer->extracted} @destpath;
 
      # Get list of files in the cache or local directory
      for my $file (($layer->get_files)) {
        if (exists $base_files{$file}) {
          # File exists in the base, compare current version with base version,
          # discard if not changed
          my $base_file = catfile $base_files{$file}->localdir, $file;
          my $used_file = catfile $used_files{$file}->localdir, $file;
          my $this_file = catfile $layer->localdir, $file;

          if (compare ($base_file, $this_file)) { # Differs
            if ($base_files{$file} eq $used_files{$file}) {
              # Base and used are the same layer, use current layer
              $used_files{$file} = $layer;

            } elsif (compare ($used_file, $this_file) == 0) {
              # Changes in used and this are the same, no update required

              # Print a message at verbose mode 2 or above
              if ($verbose > 1) {
                print &_print_override_mesg (
                  FILE   => $file,
                  LAYER0 => $base_files{$file},
                  LAYER1 => $used_files{$file},
                  LAYER2 => $layer,
                );
                print '  Same modifications, use the source in URL 1.', "\n";
              }

            } elsif ($self->{OVERRIDE}) {
              # Base and used are different, and used is not the same as this
              # Override mode, use current layer

              # Print override mode message
              if ($verbose) {
                print &_print_override_mesg (
                  FILE   => $file,
                  LAYER0 => $base_files{$file},
                  LAYER1 => $used_files{$file},
                  LAYER2 => $layer,
                );
                print '  ', $file, ' in URL 2 overrides that in URL 1.', "\n";
              }

              $used_files{$file} = $layer;

            } else {
              # Base and used are different, and used is not the same as this
              # Non-override mode, fail the extract
              w_report &_print_override_mesg (
                FILE   => $file,
                LAYER0 => $base_files{$file},
                LAYER1 => $used_files{$file},
                LAYER2 => $layer,
              );
              w_report '  Override mode is false, file in URL 1 cannot ',
                       'override file in URL 2, abort.';
              return;
            }
          }
 
        } else {
          # The first time the file is found
          $base_files{$file} = $layer;
          $used_files{$file} = $layer;
        }
      }
    }

    # Add current destination to the beginning of the destination search path
    unshift @destpath, $srcdir->{DEST} if -d $srcdir->{DEST};

    for my $file (keys %used_files) {
      # Ignore sub-directories
      if (-d catfile $used_files{$file}->localdir, $file) {
        # Print diagnostic
        if ($verbose > 1) {
          print 'Ignore subdirectory: ', $file, "\n";
          print '                Src: ', $used_files{$file}->location;
          print '@', $used_files{$file}->version unless $used_files{$file}->user;
          print "\n";
        }
        $v_count{IGNORED_SUBDIRS}++;
        next;
      }

      # Determine whether file has changed, compared with the destination
      my $diff = 1;
      for my $dir (@destpath) {
        my $old = catfile ($dir, $file);

        if (-f $old) {
          my $new = catfile ($used_files{$file}->localdir, $file);
          $diff   = compare $old, $new;
          last;
        }
      }

      if ($diff) { # copy if differs
        # Create extract destination, if required
        if (not -d $srcdir->{DEST}) {
          print 'Create directory: ', $srcdir->{DEST}, "\n" if $verbose > 1;
          my $mkdirs = mkpath $srcdir->{DEST};

          if (! -d $srcdir->{DEST} or ! -w $srcdir->{DEST}) {
            w_report $srcdir->{DEST}, ': not a writable directory, abort.';
            return;
          }

          $v_count{CREATED_DIRS} += $mkdirs;
        }

        # Set up the copy command
        my @cmd = (
          'cp',
          catfile ($used_files{$file}->localdir, $file),
          $srcdir->{DEST},
        );

        my $dest_file = catfile ($srcdir->{DEST}, $file);

        # Print diagnostic
        if ($verbose > 1) {
          print 'Update: ', $dest_file, "\n";
          print '   Src: ', $used_files{$file}->location;
          print '@', $used_files{$file}->version unless $used_files{$file}->user;
          print "\n";
        }

        # Remove old file if it exists
        unlink $dest_file if -f $dest_file;

        # Execute the copy command
        &run_command (\@cmd, TIME => $self->config->verbose > 2);

        $v_count{UPDATED_FILES}++;
      }

    }

    # Check that the destination directory does not contain any removed files
    opendir DIR, $srcdir->{DEST};
    my @dest_files = readdir DIR;
    closedir DIR;

    while (my $file = shift @dest_files) {
      next if $file =~ /^\.\.?/;                   # ignore hidden files
      next if -d catfile ($srcdir->{DEST}, $file); # ignore sub-directories

      # Check if the file exists in any of the versions
      my $exists = 0;
      for my $layer (@{ $srcdir->{STACK} }) {
        if (-f catfile ($layer->localdir, $file)) {
          $exists = 1;
          last;
        }
      }

      # File exists in destination but not in any versions...
      if (not $exists) {
        my @cmd = (
          qw/rm -f/,
          catfile ($srcdir->{DEST}, $file),
        );

        # Print diagnostic
        print 'Remove: ', catfile ($srcdir->{DEST}, $file), "\n"
          if $verbose > 1;

        # Execute the command
        &run_command (\@cmd, TIME => $self->config->verbose > 2);

        $v_count{REMOVED_FILES}++;
      }
    }
  }

  if ($verbose) {
    my %v_label = (
      CREATED_DIRS    => 'Number of directories created    : ',
      IGNORED_SUBDIRS => 'Number of ignored sub-directories: ',
      UPDATED_FILES   => 'Number of updated files          : ',
      REMOVED_FILES   => 'Number of removed files          : ',
    );
    for my $key (qw/CREATED_DIRS IGNORED_SUBDIRS UPDATED_FILES REMOVED_FILES/) {
      print $v_label{$key}, $v_count{$key}, "\n" if $v_count{$key};
    }
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $string = _print_override_mesg (
#     FILE => $file,
#     LAYER0 => $layer0,
#     LAYER1 => $layer1,
#     LAYER2 => $layer2,
#   );
#
# DESCRIPTION
#   This internal method returns a string containing an override mode message.
#
# ARGUMENTS
#   FILE   - name of the source file
#   LAYER0 - base location
#   LAYER1 - source location overridden by LOC2
#   LAYER2 - source location overriding LOC1
# ------------------------------------------------------------------------------

sub _print_override_mesg {
  my %args = @_;

  my $string = $args{FILE};
  $string .= ': modified in both URL 1 and URL 2, relative to BASE:';
  $string .= "\n";
  $string .= '  BASE : ' . $args{LAYER0}->location;
  $string .= '@' . $args{LAYER0}->version unless $args{LAYER0}->user;
  $string .= "\n";
  $string .= '  URL 1: ' . $args{LAYER1}->location;
  $string .= '@' . $args{LAYER1}->version unless $args{LAYER1}->user;
  $string .= "\n";
  $string .= '  URL 2: ' . $args{LAYER2}->location;
  $string .= '@' . $args{LAYER2}->version unless $args{LAYER2}->user;
  $string .= "\n";

  return $string;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_sort_bdeclare ();
#
# DESCRIPTION
#   This internal method sorts the declared build configuration entries,
#   filtering out repeated entries.
# ------------------------------------------------------------------------------

sub _sort_bdeclare {
  my $self = shift;

  # Get list of build configuration labels that can be declared multiple times
  my %cfg_labels   = %{ $self->config->setting ('CFG_LABEL') };
  my @cfg_keywords = split /,/, $self->config->setting ('CFG_KEYWORD');
  @cfg_keywords    = map {$cfg_labels{$_}} @cfg_keywords;

  # Filter out repeated declarations
  my @bdeclares = ();
  for my $bdeclare (reverse @{ $self->{BDECLARE} }) {
    my $label = $bdeclare->{LABEL};

    # Do not filter any declarations that can be declared multiple times
    my $unshift_ok = grep {
      uc ($label) eq $_ or index (uc ($label), $_ . '::') == 0;
    } @cfg_keywords;
    # @bdeclare contains nothing, last entry
    $unshift_ok    = 1 unless $unshift_ok or @bdeclares;
    # Check if a later entry already exists
    $unshift_ok    = 1
      unless $unshift_ok or grep {$_->{LABEL} eq $label} @bdeclares;

    # Reconstruct array from bottom up
    unshift @bdeclares, $bdeclare if $unshift_ok;
  }

  $self->{BDECLARE} = \@bdeclares;

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_write_ext_cfg ();
#
# DESCRIPTION
#   This internal method writes the expanded extract configuration file.
# ------------------------------------------------------------------------------

sub _write_ext_cfg {
  my $self = shift;

  my %cfg_labels = %{ $self->config->setting ('CFG_LABEL') };
  my %subdir     = %{ $self->config->setting ('DIR') };
  my %cfgname    = %{ $self->config->setting ('CFG_NAME') };

  # Create new config file object and assign lines to it
  my $cfg = Fcm::CfgFile->new (CONFIG => $self->config, TYPE => 'ext',);

  # Set up config file header
  $cfg->add_header ();

  # Re-use pre-extracted expanded ext cfg
  if (@{ $self->{USE} }) {
    $cfg->add_comment_block ('Other ext cfg');

    for my $reuse (@{ $self->{USE} }) {
      my $rootdir = $reuse->dest ('ROOTDIR');
      my $ext_cfg = $reuse->cfg->src;

      # Default location of build config file
      my $def_ext_cfg = catfile $rootdir, $subdir{CFG}, $cfgname{EXT};

      $cfg->add_line (
        LABEL => $cfg_labels{USE},
        VALUE => $ext_cfg eq $def_ext_cfg ? $rootdir : $ext_cfg,
      );
    }

    # Blank line
    $cfg->add_line;
  }

  # Destination directories, config file, etc
  my $dest = $self->{DEST};

  $cfg->add_comment_block ('Destination');

  $cfg->add_line (
    LABEL => $cfg_labels{DEST}{ROOTDIR},
    VALUE => $dest->{ROOTDIR},
  );

  for my $label (qw/CFG SRC CACHE/) {
    my $dir = $label . 'DIR';

    if ($dest->{$dir} ne catfile $dest->{ROOTDIR}, $subdir{$label}) {
      $cfg->add_line (
        LABEL => $cfg_labels{DEST}{$dir},
        VALUE => $dest->{$dir},
      );
    }
  }

  for my $name (qw/BLD EXT/) {
    my $label = $name . '_CFG';

    if ($dest->{$label} ne catfile $dest->{CFGDIR}, $cfgname{$name}) {
      $cfg->add_line (
        LABEL => $cfg_labels{DEST}{$label},
        VALUE => $dest->{$label},
      );
    }
  }

  # Blank line
  $cfg->add_line;

  # Remote destination directories, config file, etc
  if ($self->{RDEST}{MACHINE}) {
    my $rdest = $self->{RDEST};

    $cfg->add_comment_block ('Remote destination');

    for my $label (qw/MACHINE LOGNAME ROOTDIR/) {
      $cfg->add_line (
        LABEL => $cfg_labels{RDEST}{$label},
        VALUE => $rdest->{$label},
      );
    }

    for my $label (qw/CFG SRC/) {
      my $dir = $label . 'DIR';
      if ($rdest->{$dir} ne catfile $rdest->{ROOTDIR}, $subdir{$label}) {
        $cfg->add_line (
          LABEL => $cfg_labels{RDEST}{$dir},
          VALUE => $rdest->{$dir},
        );
      }
    }

    for my $name (qw/BLD EXT/) {
      my $label = $name . '_CFG';

      if ($rdest->{$label} ne catfile $rdest->{CFGDIR}, $cfgname{$name}) {
        $cfg->add_line (
          LABEL => $cfg_labels{RDEST}{$label},
          VALUE => $rdest->{$label},
        );
      }
    }

    $cfg->add_line (
      LABEL => $cfg_labels{MIRROR},
      VALUE => $self->config->setting (qw/TOOL MIRROR/),
    );

    # Blank line
    $cfg->add_line;
  }

  if ($self->{OVERRIDE}) {
    $cfg->add_line (
      LABEL => $cfg_labels{OVERRIDE},
      VALUE => $self->{OVERRIDE} ? 1 : 0,
    );
    $cfg->add_line;
  }

  # Source directories
  $cfg->add_comment_block ('Source directories');

  # Set up lines in the ext cfg
  my @lines = ();
  for my $my_label (keys %{ $self->{SRCDIRS} }) {
    for my $layer (@{ $self->{SRCDIRS}{$my_label}{STACK} }) {
      next if $layer->extracted;

      my $tag = $layer->package . '::' . $layer->tag;

      # Repository
      my $exists = grep {
        $_->{LABEL} eq $cfg_labels{REPOS} . '::' . $tag;
      } @lines;
      push @lines, {
        LABEL   => $cfg_labels{REPOS} . '::' . $tag,
        VALUE   => $layer->reposroot,
      } if not $exists;

      # Version
      $exists = grep {
        $_->{LABEL} eq $cfg_labels{VERSION} . '::' . $tag;
      } @lines;
      push @lines, {
        LABEL   => $cfg_labels{VERSION} . '::' . $tag,
        VALUE   => $layer->version,
      } unless $layer->user or $exists;

      # Source directory
      my ($pcks, $path);

      if ($layer->reposroot) {
        # Repository root declaration exists, print relative path
        if ($layer->location eq $layer->reposroot) {
          $path  = '';

        } else {
          $path  = substr ($layer->location, length ($layer->reposroot) + 1);
        }
        my @pcks = split /::/, $my_label;
        shift @pcks;

        if (join ('::', @pcks) eq join ('::', File::Spec->splitdir ($path))) {
          # Print top package name if relative path matches sub-package name
          $pcks = $layer->package;

        } else {
          # Print full sub-package name otherwise
          $pcks = $my_label;
        }

      } else {
        # No repository root declaration
        # Print full path and full sub-package name
        $path = $layer->location;
        $pcks = $my_label;
      }

      my $length = $layer->reposroot ? length ($layer->reposroot) + 1 : 0;
      push @lines, {
        LABEL   => join ('::', ($cfg_labels{SRCDIR}, $pcks, $layer->tag)),
        VALUE   => $path,
      };
    }
  }

  # Sort lines for specifying repository, version and source directories
  @lines = sort {
    my $rep_label = $cfg_labels{REPOS};
    my $ver_label = $cfg_labels{VERSION};

    if ($a->{LABEL} =~ /^$rep_label/) {

      # Repository labels
      if ($b->{LABEL} =~ /^$rep_label/) {
        $a->{LABEL} cmp $b->{LABEL} or $a->{VALUE} cmp $b->{VALUE};
      } else {
        -1;
      }

    } elsif ($a->{LABEL} =~ /^$ver_label/) {

      # Version labels
      if ($b->{LABEL} =~ /^$rep_label/) {
        1;
      } elsif ($b->{LABEL} =~ /^$ver_label/) {
        $a->{LABEL} cmp $b->{LABEL} or $a->{VALUE} cmp $b->{VALUE};
      } else {
        -1;
      }
    } else {

      # Source directories labels
      if ($b->{LABEL} =~ /^(?:$rep_label|$ver_label)/) {
        1;
      } else {
        $a->{LABEL} cmp $b->{LABEL} or $a->{VALUE} cmp $b->{VALUE};
      }

    }
  } @lines;

  # Add lines for specifying repository, version and source directories
  while (my $line = shift @lines) {
    $cfg->add_line (
      LABEL => $line->{LABEL},
      VALUE => $line->{VALUE},
    );
  }

  # Add declared bld cfg entries
  if (@{ $self->{BDECLARE} }) {
    # Blank line
    $cfg->add_line;

    $cfg->add_comment_block ('Declared bld cfg entries');
    for my $bdeclare (@{ $self->{BDECLARE} }) {
      $cfg->add_line (
        LABEL => $cfg_labels{BDECLARE} . '::' . $bdeclare->{LABEL},
        VALUE => $bdeclare->{VALUE},
      );
    }
  }

  # Print lines to config file
  $cfg->print_cfg ($self->{DEST}{EXT_CFG});

  return 1;

}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_write_bld_cfg ();
#
# DESCRIPTION
#   This internal method writes the build configuration file.
# ------------------------------------------------------------------------------

sub _write_bld_cfg {
  my $self = shift;

  my %cfg_labels = %{ $self->config->setting ('CFG_LABEL') };
  my %subdir     = %{ $self->config->setting ('DIR') };
  my %cfgname    = %{ $self->config->setting ('CFG_NAME') };

  # Create new config file object and assign lines to it
  my $cfg = Fcm::CfgFile->new (CONFIG => $self->config, TYPE => 'bld');

  # Set up config file header
  $cfg->add_header ();

  # Pre-compile source
  if (@{ $self->{USE} }) {
    $cfg->add_comment_block ('Pre-compile source');

    for my $reuse (@{ $self->{USE} }) {
      my $rootdir;
      my $bld_cfg;

      if ($self->{RDEST}{MACHINE}) {
        $rootdir = $reuse->rdest ('ROOTDIR');
        $bld_cfg = $reuse->rdest ('BLD_CFG');
      } else {
        $rootdir = $reuse->dest ('ROOTDIR');
        $bld_cfg = $reuse->dest ('BLD_CFG');
      }

      # Default location of build config file
      my $def_bld_cfg = catfile $rootdir, $subdir{CFG}, $cfgname{BLD};

      $cfg->add_line (
        LABEL => $cfg_labels{USE},
        VALUE => $bld_cfg eq $def_bld_cfg ? $rootdir : $bld_cfg,
      );
    }

    # Blank line
    $cfg->add_line;
  }

  # Add declared bld cfg entries
  if (@{ $self->{BDECLARE} }) {
    $cfg->add_comment_block ('Declared build options...');

    my @bdeclares = sort {$a->{LABEL} cmp $b->{LABEL}} @{ $self->{BDECLARE} };
    for my $bdeclare (@bdeclares) {
      $cfg->add_line (
        LABEL => $bdeclare->{LABEL},
        VALUE => $bdeclare->{VALUE},
      );
    }

    # Blank line
    $cfg->add_line;
  }

  # Add source directories to config file
  $cfg->add_comment_block ('Project directory tree');

  my $dest = $self->{RDEST}{MACHINE} ? $self->{RDEST} : $self->{DEST};
  $cfg->add_line (
    LABEL => $cfg_labels{DIR} . '::ROOT',
    VALUE => $dest->{ROOTDIR},
  );
  for my $label (qw/SRC CFG/) {
    my $dir = $label . 'DIR';
    if ($dest->{$dir} ne catfile $dest->{ROOTDIR}, $subdir{$label}) {
      $cfg->add_line (
        LABEL => $cfg_labels{DIR} . '::' . $label,
        VALUE => $dest->{$dir},
      );
    }
  }

  # Blank line
  $cfg->add_line;

  # Add source directories to config file
  $cfg->add_comment_block ('Source directories');

  $cfg->add_line (LABEL => $cfg_labels{SEARCH_SRC}, VALUE => '0',);
  $cfg->add_line;

  for my $srcdir (sort keys %{ $self->{SRCDIRS} }) {

    if (-d $self->{SRCDIRS}{$srcdir}{DEST}) {
      # Check whether pre-extracted source exists
      my $pre_extracted = grep {
        $_->extracted;
      } @{ $self->{SRCDIRS}{$srcdir}{STACK} };

      # Source directory
      my $dest = undef;
      if ($self->{RDEST}{MACHINE}) {
        my $base = substr $self->{SRCDIRS}{$srcdir}{DEST},
                          length ($self->{DEST}{SRCDIR}) + 1;
        $dest    = catfile $self->{RDEST}{SRCDIR}, $base;
      } else {
        $dest = $self->{SRCDIRS}{$srcdir}{DEST}
      }

      # Source directory label
      my $label = join '::', ($cfg_labels{SRCDIR}, $srcdir);

      $cfg->add_line (LABEL => $label, VALUE => $dest,)
    }

  }

  # Print lines to config file
  $cfg->print_cfg ($self->{DEST}{BLD_CFG});

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_mirror_extract ();
#
# DESCRIPTION
#   This internal method mirrors the current extract to a remote machine.
# ------------------------------------------------------------------------------

sub _mirror_extract {
  my $self = shift;

  # Needs mirroring only if remote machine is set
  return unless $self->{RDEST}{MACHINE};

  my $verbose = $self->config->verbose;

  my $mirror = $self->config->setting (qw/TOOL MIRROR/);

  if ($mirror eq 'rdist') {
    # Use "rdist" to mirror extract

    # Variable for "remote_logname@remote_machine"
    my $rhost = $self->{RDEST}{LOGNAME} . '@' . $self->{RDEST}{MACHINE};

    # Print distfile content to temporary file
    my @distfile = ();
    for my $my_label (qw/BLD_CFG EXT_CFG SRCDIR/) {
      push @distfile, '( ' . $self->{DEST}{$my_label} . ' ) -> ' . $rhost . "\n";
      push @distfile, '  install ' . $self->{RDEST}{$my_label} . ';' . "\n";
    }

    # Set up mirroring command (use "rdist" at the moment)
    my $command = 'rdist -R';
    $command   .= ' -q' unless $verbose > 1;
    $command   .= ' -f - 1>/dev/null';

    # Diagnostic
    my $croak = 'Cannot execute "' . $command . '"';
    if ($verbose > 2) {
      print timestamp_command ($command, 'Start');
      print '  ', $_ for (@distfile);
    }

    # Execute the mirroring command
    open COMMAND, '|-', $command or croak $croak, ' (', $!, '), abort';
    for my $line (@distfile) {
      print COMMAND $line;
    }
    close COMMAND or croak $croak, ' (', $?, '), abort';

    # Diagnostic
    print timestamp_command ($command, 'End  ') if $verbose > 2;

  } elsif ($mirror eq 'rsync') {
    # Use "rsync" to mirror extract

    my $rsh = $self->config->setting (qw/TOOL REMOTE_SHELL/);

    # Variable for "remote_logname@remote_machine"
    my $rhost = $self->{RDEST}{LOGNAME} . '@' . $self->{RDEST}{MACHINE};

    for my $my_label (qw/BLD_CFG EXT_CFG SRCDIR/) {
      my $rdir = dirname $self->{RDEST}{$my_label}; # remote container directory

      {
        # Create remote container directory with remote shell command
        my @command = (
          $rsh, $self->{RDEST}{MACHINE}, '-n', '-l', $self->{RDEST}{LOGNAME},
          qw/mkdir -p/, $rdir,
        );

        # Execute command
        &run_command (\@command, TIME => $verbose > 2);
      }

      {
        # Build the rsync command
        my @command = qw/rsync -a --exclude='.*' --delete-excluded/;
        push @command, '-v' if $verbose > 2;
        push @command, $self->{DEST}{$my_label};
        push @command, $rhost . ':' . $rdir;

        # Execute command
        &run_command (\@command, TIME => $verbose > 2);
      }
    }

  } else {
    w_report $mirror, ': unknown mirroring tool, abort.';
    return;
  }

  return 1;
}

# ------------------------------------------------------------------------------

1;

__END__
