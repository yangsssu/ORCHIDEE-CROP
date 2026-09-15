#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::Build
#
# DESCRIPTION
#   The main purpose of this class is to process the build configuration,
#   generate the required files for the build and invoke make to create the
#   build.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::Build;

# Standard pragma
use strict;
use warnings;

# Standard modules
use Carp;
use Cwd;
use File::Basename;
use File::Path;
use File::Spec::Functions;

# FCM component modules
use Fcm::CfgFile;
use Fcm::SrcPackage;
use Fcm::BuildTask;
use Fcm::Util;
use Fcm::Timer;

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $bld = Fcm::Build->new (
#     CONFIG  => $config,
#     CFG_SRC => $cfg_src,
#   );
#
# DESCRIPTION
#   This method constructs a new instance of the Fcm::Build class.
#
# ARGUMENTS
#   CONFIG     - reference to a Fcm::Config instance
#   CFG_SRC    - source path to the build configuration file
# ------------------------------------------------------------------------------

sub new {
  my $this  = shift;
  my %args  = @_;
  my $class = ref $this || $this;

  my $cfg    = exists $args{CFG_SRC} ? $args{CFG_SRC} : undef;
  my $config = exists $args{CONFIG}  ? $args{CONFIG}  : &main::cfg;

  my $self  = {
    CONFIG   => $config,            # configuration settings
    CFG      => Fcm::CfgFile->new ( # bld cfg
      TYPE   => 'bld',              # config file type
      SRC    => $cfg,               # source path of the bld cfg
      CONFIG => $config,            # configuration settings
    ),
    NAME     => '',                 # name of this build
    DIR      => {                   # directory tree of this build
      ROOT    => '',                # root directory of this build
    },
    PATH     => {},                 # search paths of this build
    SEARCH   => 1,                  # search for source directories in src/?
    SRCDIR   => {},                 # source directories of this build
    PP       => {},                 # pre-process flags
    PACKAGE  => {},                 # source directory packages of this build
    TARGET   => [],                 # targets of this build
    USE      => [],                 # list of inherited builds
    INHERIT  => {                   # inheritance flags
      SRCDIR => 1,                  # inherit source directories?
      PP     => 1,                  # inherit pre-process flags?
      TARGET => 0,                  # inherit targets?
    },
    LIB      => {'' => ''},         # name of libraries
    LOCK     => undef,              # lock file
  };
  bless $self, $class;
  return $self;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->DESTROY;
#
# DESCRIPTION
#   This method is called automatically when a Fcm::Build object is
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
#   $config = $bld->config;
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
#   $cfgfile = $bld->cfg;
#
# DESCRIPTION
#   This method returns a reference to a Fcm::CfgFile instance for the build
#   configuration file.
# ------------------------------------------------------------------------------

sub cfg {
  my $self = shift;

  return $self->{CFG};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   %allpcks = $bld->allpcks ();
#
# DESCRIPTION
#   This method returns a hash table with keys representing all the packages
#   declared in the current build. The value of each element in the hash is a
#   reference to a list of children of the current package.
# ------------------------------------------------------------------------------

sub allpcks {
  my $self        = shift;
  my %allpcks = ();

  for my $pckname (keys %{ $self->{PACKAGE} }) {
    $allpcks{$pckname} = [];
  }

  for my $pckname (keys %{ $self->{PACKAGE} }) {
    my @names = split /__/, $pckname;

    my $cur = $pckname;
    while ($cur) {
      pop @names;
      my $depend = @names ? join '__', @names : '';
      $allpcks{$depend} = [] unless exists $allpcks{$depend};

      push @{ $allpcks{$depend} }, $cur
        unless grep {$_ eq $cur} @{ $allpcks{$depend} };

      $cur = $depend;
    }
  }

  return %allpcks;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = $bld->build (
#     [ARCHIVE     => $archive,]
#     [FULL        => $full,]
#     [IGNORE_LOCK => $ignore_lock,]
#     [JOBS        => $jobs,]
#     [STAGE       => $stage,]
#     [TARGETS     => \@targets,]
#   );
#
# DESCRIPTION
#   This method performs a build based on the current configuration. The
#   method returns 1 on success.
#
# ARGUMENTS
#   ARCHIVE      - If set to "true", invoke the "archive" mode. Most build files
#                  and directories created by this build will be archived using
#                  the "tar" command. If not set, the default is not to invoke
#                  the "archive" mode.
#   FULL         - If set to "true", invoke the build in "full" mode. Build files
#                  and directories created by previous builds in the same
#                  location will be removed before the current build is
#                  performed. If not set, the default is to perform the build
#                  in "incremental" mode.
#   IGNORE_LOCK  - If set to "true", it ignores any lock files that may exist in
#                  the build root directory. 
#   JOBS         - Specify number of jobs that can be handled by "make". If set,
#                  the value must be a natural integer. If not set, the default
#                  value is 1 (i.e. run "make" in serial mode).
#   STAGE        - If set, it should be an integer number or a recognised
#                  keyword or abbreviation. If set, the build is performed up
#                  to the named stage. If not set, the default is to perform
#                  all stages of the build. Allowed values are:
#                  1, setup or s
#                  2, pre_process or pp
#                  3, generate_dependency or gd
#                  4, generate_interface or gi
#                  5, all, a, make or m
#   TARGETS      - Specify targets to be built. If set, these targets will be
#                  built instead of the ones specified in the build
#                  configuration file.
# ------------------------------------------------------------------------------

sub build {
  my $self = shift;
  my %args = @_;

  # Process arguments
  my $archive     = exists $args{ARCHIVE}     ? $args{ARCHIVE}     : 0;
  my $full        = exists $args{FULL}        ? $args{FULL}        : 0;
  my $ignore_lock = exists $args{IGNORE_LOCK} ? $args{IGNORE_LOCK} : 0;
  my $jobs        = exists $args{JOBS}        ? $args{JOBS}        : 1;
  my $stage       = exists $args{STAGE}       ? $args{STAGE}       : 5;
  my $targets     = exists $args{TARGETS}     ? $args{TARGETS}     : [qw/all/];

  # Resolve named stages
  $stage = 5 unless $stage;
  if ($stage !~ /^\d$/) {
    my %stagenames = (
      'S(?:ETUP)?'                      => 1,
      'P(?:RE)?_?P(?:ROCESS)?'          => 2,
      'G(?:ENERATE)?_?D(?:ENPENDENCY)?' => 3,
      'G(?:ENERATE)?_?I(?:NTERFACE)?'   => 4,
      '(?:A(?:LL)|M(?:AKE)?)'           => 5,
    );

    for my $name (keys %stagenames) {
      if ($stage =~ /$name/i) {
        $stage = $stagenames{$name};
        last;
      }
    }

    if ($stage !~ /^\d$/) {
      w_report 'Warning: invalid build stage: ', $stage, ', default to "5"';
      $stage = 5;
    }
  }

  # Get verbose mode
  my $verbose = $self->config->verbose;

  # Stage 1: setup
  my $date = localtime;
  print 'Build command started on ', $date, '.', "\n" if $verbose;
  my $otime = time;

  print '->Setup              : start', "\n" if $verbose;
  my $stime = time;

  # Read configurations
  my $rc = $self->decipher_cfg;

  # Check directories are set
  $rc = $self->check_dir if $rc;

  # Check for lock files
  $rc = $self->check_lock if $rc and not $ignore_lock;

  # Set a lock file
  $rc = $self->_set_lock if $rc;

  # Create build root directory if necessary
  $rc = $self->_create_build_dir if $rc;

  # Set up directory inheritance
  $rc = $self->_setup_dir (FULL => $full) if $rc;

  # Set up source directories, targets, etc
  $rc = $self->_update_bld_info if $rc;

  # Set up PP flag cache
  $rc = $self->_update_pp_info if $rc;

  # Set up build tools cache
  $rc = $self->_update_tool_info if $rc;

  # Set up regenerate make rule cache
  $rc = $self->_update_regen_rule_info if $rc;

  my $ftime = time;
  my $s_str = $ftime - $stime > 1 ? 'seconds' : 'second';
  print '->Setup              : ', $ftime - $stime, ' ', $s_str, "\n";

  # Stage 2: Pre-process
  if ($rc and $stage >= 2) {
    print '->Pre-process        : start', "\n" if $verbose;
    my $stime = time;

    $rc = $self->_pre_process;

    $ftime = time;
    $s_str = $ftime - $stime > 1 ? 'seconds' : 'second';
    print '->Pre-process        : ', $ftime - $stime, ' ', $s_str, "\n";
  }

  # Stage 3: Scan dependency and write make rules
  if ($rc and $stage >= 3) {
    print '->Scan dependency    : start', "\n" if $verbose;
    my $stime = time;

    $rc = $self->_scan_dependency;
    $rc = $self->_write_make_rules if $rc;
    $rc = $self->_write_makefile if $rc;

    $ftime = time;
    $s_str = $ftime - $stime > 1 ? 'seconds' : 'second';
    print '->Scan dependency    : ', $ftime - $stime, ' ', $s_str, "\n";
  }

  # Stage 4: Generate Fortran 9x interface block
  if ($rc and $stage >= 4) {
    print '->Generate interface : start', "\n" if $verbose;
    my $stime = time;

    $rc = $self->_generate_f9x_interface;

    $ftime = time;
    $s_str = $ftime - $stime > 1 ? 'seconds' : 'second';
    print '->Generate interface : ', $ftime - $stime, ' ', $s_str, "\n";
  }

  # Stage 5: Make the build
  if ($rc and $stage >= 5) {
    print '->Make               : start', "\n" if $verbose;
    my $stime = time;

    $rc = $self->_invoke_make (
      TARGETS => $targets,
      JOBS    => $jobs,
      ARCHIVE => $archive,
    );

    # Remove empty build directories
    $rc = $self->_remove_empty_dirs () if $rc;

    # Create TAR archives if necessary
    $rc = $self->_tar_build_dirs () if $rc and $archive;

    # Create run time environment script if necessary
    $rc = $self->_create_runenv_script () if $rc;

    # Create exclude dependency configurations for libraries
    $rc = $self->_create_lib_excl_dep () if $rc;

    $ftime = time;
    $s_str = $ftime - $stime > 1 ? 'seconds' : 'second';
    print '->Make               : ', $ftime - $stime, ' ', $s_str, "\n";
  }

  if ($verbose) {
    $s_str = $ftime - $otime > 1 ? 'seconds' : 'second';
    print '->TOTAL              : ', $ftime - $otime, ' ', $s_str, "\n";
  }

  $date = localtime;
  if ($rc) {
    print 'Build command finished on ', $date, '.', "\n" if $verbose;

  } else {
    e_report 'Build command failed on ', $date, '.';
  }

  return $rc;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $bld->decipher_cfg ();
#
# DESCRIPTION
#   This method deciphers the build configuration file and assigns the
#   configurations to the variables of the current build.
# ------------------------------------------------------------------------------

sub decipher_cfg {
  my $self = shift;

  my $read = $self->cfg->read_cfg;

  # Check config file type
  if ($read) {
    if ($self->cfg->type ne 'bld') {
      w_report 'Error: ', $self->cfg->src, ': not a build config file.';
      return;
    }

  } else {
    return;
  }

  my %cfg_label = %{ $self->config->setting ('CFG_LABEL') };
  my %tools     = %{ $self->config->setting ('TOOL') };

  # Get lines from cfg file
  my @cfg_lines  = $self->cfg->lines;

  LINE: for my $line (@cfg_lines) {
    # Label and value of each line
    my $label = $line->{LABEL};
    my $value = $line->{VALUE};

    next LINE unless $label; # ignore blank or comment line

    # Strip out BLD prefix from all labels
    my $prefix = $cfg_label{BDECLARE} . '::';
    $label = substr ($label, length ($prefix))
      if index (uc ($label), $prefix) == 0;

    next LINE unless $label; # ignore blank or comment line

    # Configuration file type/version, ignore
    next LINE if uc $label eq $cfg_label{CFGFILE}{TYPE};
    next LINE if uc $label eq $cfg_label{CFGFILE}{VERSION};

    # User variable, ignore
    next LINE if index (uc ($label), '%') == 0;

    # Build name
    if (uc $label eq $cfg_label{NAME}) {
      $self->{NAME} = $value;
      next LINE;
    }

    # Build directory tree
    $prefix = $cfg_label{DIR} . '::';
    if (index (uc ($label), $prefix) == 0) {
      my $name = substr uc ($label), length ($prefix);
      $self->{DIR}{$name} = expand_tilde $value;
      next LINE;
    }

    # Source directory
    $prefix = $cfg_label{SRCDIR} . '::';
    if (index (uc ($label), $prefix) == 0) {
      my $name = substr $label, length ($prefix);
      $name    =~ s/::/__/g;
      $self->{SRCDIR}{$name} = expand_tilde $value;
      next LINE;
    }

    # Automatic source directory search?
    if (uc $label eq $cfg_label{SEARCH_SRC}) {
      $self->{SEARCH} = $value;
      next LINE;
    }

    # Pre-process flag, directory/file requires pre-processing before all tasks
    $prefix = $cfg_label{PP};
    if (index (uc ($label), $prefix) == 0) {
      my @flds = split /::/, $label;
      my $name = uc shift @flds;
      $name    = join '__', ($name, @flds) if @flds;
      $self->{PP}{$name} = $value;
      next LINE;
    }

    # Specify name of top level or package library
    $prefix = $cfg_label{LIB};
    if (index (uc ($label), $prefix) == 0) {
      my @flds = split /::/, $label;
      shift @flds;
      my $name = @flds ? join ('__', @flds) : '';
      $self->{LIB}{$name} = $value;

      next LINE;
    }

    # Specify extra executable dependencies and BLOCKDATA dependency
    for my $name (qw/EXE_DEP BLOCKDATA/) {
      $prefix = $cfg_label{$name};

      if (index (uc ($label), $prefix) == 0) {
        my @flds = split /::/, $label;
        shift @flds;
        my $target = @flds ? $flds[0] : '';
        my @deps   = split /\s+/, $value;

        # If $value is a null string, set executable to depend on all objects
        if (not @deps) {
          if ($name eq 'BLOCKDATA') {

            # Label not recognised
            w_report 'Warning: ', $line->{SRC}, ': LINE ', $line->{NUMBER},
                     ': "', $label, '" declaration must have a value';
            next LINE;

          } else {
            push @deps, '';
          }
        }

        for my $dep (@deps) {
          $dep =~ s/::/__/g;

          $self->config->assign_setting (
            LABELS => [$name, $target, $dep],
            VALUE  => 1,
          );
        }

        next LINE;
      }
    }

    # Build target
    if (uc $label eq $cfg_label{TARGET}) {
      push @{ $self->{TARGET} }, split (/\s+/, $value);
      next LINE;
    }

    # Rename a main program target
    $prefix = $cfg_label{EXE_NAME};
    if (index (uc ($label), $prefix) == 0) {
      my @flds = split /::/, $label;
      shift @flds;
      my $name = shift @flds;

      if ($name and $value) {
        $self->config->assign_setting (
          LABELS => ['EXE_NAME', $name],
          VALUE  => $value,
        );

        next LINE;
      }
    }

    # Build tool
    $prefix = $cfg_label{TOOL} . '::';
    if (index (uc ($label), $prefix) == 0) {
      my $name = substr $label, length ($prefix);
      my @flds = split /::/, $name;

      $name = uc (shift @flds);

      if (exists $tools{$name}) {
        $name = join '__', ($name, @flds) if @flds;

        $self->config->assign_setting (
          LABELS => ['TOOL', $name],
          VALUE  => $value,
        );
        next LINE;
      }
    }

    # File name extension and type
    for my $name (qw/INFILE_EXT OUTFILE_EXT/) {
      $prefix = $cfg_label{$name};
      if (index (uc ($label), $prefix) == 0) {
        my $key = (split /::/, $label)[1];
        $key    = uc $key if $name eq 'OUTFILE_EXT';

        my $val = ($name eq 'INFILE_EXT') ? uc $value : $value;

        $self->config->assign_setting (
          LABELS => [$name, $key],
          VALUE  => $val,
        );
        next LINE;
      }
    }

    # Dependency scan exclusion
    $prefix = $cfg_label{EXCL_DEP};
    if (index (uc ($label), $prefix) == 0) {
      my @flds = split /::/, $label;
      shift @flds;

      my $pk = @flds ? join ('__', @flds) : '';
      $self->config->assign_setting (
        LABELS => ['EXCL_DEP', uc ($value), $pk],
        VALUE  => 1,
      );
      next LINE;
    }

    # Use (inherit from) another build
    if (uc $label eq $cfg_label{USE}) {
      my $use = Fcm::Build->new (
        CONFIG  => $self->config,
        CFG_SRC => expand_tilde ($value),
      );
      $use->decipher_cfg;
      $use->check_dir;
      push @{ $self->{USE} }, $use;
      next LINE;
    }

    # Inheritance flag
    $prefix = $cfg_label{INHERIT} . '::';
    if (index (uc ($label), $prefix) == 0) {
      my $name = substr $label, length ($prefix);
      my @flds = split /::/, $name;

      $name = uc (shift @flds);

      for my $flag (qw/SRCDIR PP LIB TARGET/) {
        if ($name eq $cfg_label{$flag}) {
          $name = @flds ? join ('__', ($flag, @flds)) : $flag;
          $self->{INHERIT}{$name} = $value;
          next LINE;
        }
      }
    }

    # Label not recognised
    w_report 'ERROR: ', $line->{SRC}, ': LINE ', $line->{NUMBER}, ': label "',
             $label, '" not recognised';
    return;
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $bld->check_dir ();
#
# DESCRIPTION
#   This method checks whether the build directories are set correctly.
# ------------------------------------------------------------------------------

sub check_dir {
  my $self = shift;

  # Make sure build root directory is set
  if (not $self->{DIR}{ROOT}) {
    w_report 'Error: build root directory not set.';
    return;
  }

  # Set value of build sub-directories if necessary
  for my $name (keys %{ $self->config->setting ('DIR') }) {
    next if $self->{DIR}{$name};

    $self->{DIR}{$name} = catfile (
      $self->{DIR}{ROOT},
      $self->config->setting ('DIR', $name),
    );
  }

  # Search src/ sub-directory if necessary
  if ($self->{SEARCH} and -d $self->{DIR}{SRC}) {
    my %dir = find_srcdir ($self->{DIR}{SRC});
    for my $name (keys %dir) {
      $self->{SRCDIR}{$name} = $dir{$name} unless $self->{SRCDIR}{$name};
    }
  }

  # Expand source directory paths if necessary
  for my $name (keys %{ $self->{SRCDIR} }) {
    if ($self->{SRCDIR}{$name} =~ /^\w/) {
      my $src_search  = catfile $self->{DIR}{SRC} , $self->{SRCDIR}{$name};
      my $root_search = catfile $self->{DIR}{ROOT}, $self->{SRCDIR}{$name};

      if ($self->{DIR}{SRC} and -d $src_search) {
        $self->{SRCDIR}{$name} = $src_search;

      } elsif (-d $root_search) {
        $self->{SRCDIR}{$name} = $root_search;

      } else {
        w_report 'Warning: cannot locate declared source directory: ',
                 $self->{SRCDIR}{$name};
        next;
      }
    }
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $bld->check_lock ();
#
# DESCRIPTION
#   This method checks whether a lock is set in the current build.
# ------------------------------------------------------------------------------

sub check_lock {
  my $self = shift;

  my $rootdir  = $self->{DIR}{ROOT};
  my $lock_ext = catfile ($rootdir, $self->config->setting (qw/MISC LOCK_EXT/));
  my $lock_bld = catfile ($rootdir, $self->config->setting (qw/MISC LOCK_BLD/));

  # Always throw error if extract lock exists
  if (-e $lock_ext) {
    w_report 'ERROR: extract lock file exists: ', $lock_ext, ',';
    w_report '       an extract may be running at ', $rootdir, ', abort.';
    return;
  }

  # Always throw error if build lock exists
  if (-e $lock_bld) {
    w_report 'ERROR: build lock file exists: ', $lock_bld, ',';
    w_report '       a build may be running at ', $rootdir, ', abort.';
    return;
  }

  # Check locks in inherited build
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
#   This method sets a lock is set in the current build.
# ------------------------------------------------------------------------------

sub _set_lock {
  my $self = shift;

  $self->{LOCK} = catfile (
    $self->{DIR}{ROOT}, $self->config->setting (qw/MISC LOCK_BLD/),
  );

  &touch_file ($self->{LOCK});

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_setup_dir (FULL => $full);
#
# DESCRIPTION
#   This internal method sets up the build directories.
# ------------------------------------------------------------------------------

sub _setup_dir {
  my $self = shift;
  my %args = @_;

  my $full = exists $args{FULL} ? $args{FULL} : 0;

  my $tar      = $self->config->setting (qw/OUTFILE_EXT TAR/);
  my @tar_dirs = split /,/, $self->config->setting (qw/TAR_DIRS/);
  my $verbose  = $self->config->verbose;

  if ($full) {
    # Remove sub-directories/archives created from previous builds
    for my $name (qw/BIN BLD DONE ETC FLAGS INC LIB PPSRC OBJ TMP/) {
      &run_command ([qw/rm -rf/, $self->{DIR}{$name}], PRINT => $verbose)
        if -d $self->{DIR}{$name};

      &run_command ([qw/rm -f/, $self->{DIR}{$name} . $tar], PRINT => $verbose)
        if -f $self->{DIR}{$name} . $tar;
    }

    # Remove cache
    my @files;
    if (-d $self->{DIR}{CACHE} and opendir DIR, $self->{DIR}{CACHE}) {
      @files = grep {$_ ne '.' and $_ ne '..'} readdir 'DIR';
      closedir DIR;
    }

    my $extension = '\\' . $self->config->setting (qw/CACHE PCKFILE/) . '|' .
                    '\\' . $self->config->setting (qw/CACHE PCKPPDEPEND/) . '|' .
                    '\\' . $self->config->setting (qw/CACHE PCKDEPEND/);


    for my $file (@files) {
      next unless $file eq $self->config->setting (qw/CACHE BLDTOOL/) or
                  $file eq $self->config->setting (qw/CACHE PPOPTION/) or
                  $file eq $self->config->setting (qw/CACHE EXE_DEP/) or
                  $file =~ /$extension$/;

      my $path = File::Spec->catfile ($self->{DIR}{CACHE}, $file);
      &run_command ([qw/rm -f/, $path], PRINT => $verbose);
    }

  } else {
    # Extract archives if necessary
    for my $name (@tar_dirs) {
      my $tar_file = $self->{DIR}{$name} . $tar;

      if (-f $tar_file) {
        &run_command ([qw/tar -x -f/, $tar_file], PRINT => $verbose > 1);
        &run_command ([qw/rm -f/, $tar_file], PRINT => $verbose > 1);
      }
    }
  }

  # Set up search paths
  for my $name (keys %{ $self->{DIR} }) {
    $self->{PATH}{$name} = [$self->_get_inherited_paths ($name)];

    $self->config->assign_setting (
      LABELS => ['PATH', $name],
      VALUE  => $self->{PATH}{$name},
    )
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_update_bld_info ();
#
# DESCRIPTION
#   This internal method sets up the inheritance relationship for source
#   directories and targets, and other configurations required by the build.
# ------------------------------------------------------------------------------

sub _update_bld_info {
  my $self = shift;

  # Set up build targets
  $self->{TARGET} = [$self->_get_inherited_items ('TARGET')];

  # Set up PP switches
  $self->{PP} = {$self->_get_inherited_items ('PP')};

  # Set up source directory packages for this build
  my %srcdir = $self->_get_inherited_items ('SRCDIR');
  for my $name (keys %srcdir) {
    my $package = Fcm::SrcPackage->new (
      CONFIG     => $self->config,
      NAME       => $name,
      CURRENT    => exists $self->{SRCDIR}{$name},
      REQUIREPP  => $self->_require_pp ($name),
      SEARCHPATH => [$self->_get_inherited_srcdirs ($name)],
    );

    $package->update_file_info ();

    $self->{PACKAGE}{$name} = $package;
  }

  # Set up runtime dependency scan patterns
  my %dep_pattern = %{ $self->config->setting ('DEP_PATTERN') };
  for my $key (keys %dep_pattern) {
    my $pattern = $dep_pattern{$key};

    while ($pattern =~ /##([\w:]+)##/g) {
      my $match = $1;
      my $val   = $self->config->setting (split (/::/, $match));

      last unless defined $val;
      $val =~ s/\./\\./;

      $pattern =~ s/##$match##/$val/;
    }

    $self->config->assign_setting (
      LABELS => ['DEP_PATTERN', $key],
      VALUE  => $pattern,
    ) unless $pattern eq $dep_pattern{$key};
  }

  # Set up top level library name
  {
    $self->{LIB} = {$self->_get_inherited_items ('LIB')};

    my $lib = $self->{LIB}{''};
    $lib    = ($self->{NAME} ? $self->{NAME} : 'fcm_default') unless $lib;
    $self->{LIB}{''} = $lib;
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_update_regen_rule_info ();
#
# DESCRIPTION
#   This internal method compares the current EXE_DEP, BLOCKDATA and EXE_NAME
#   declarations and the previous. If changed, the REGEN_MAKERULE flag will be
#   set to true and the cache will be updated.
# ------------------------------------------------------------------------------

sub _update_regen_rule_info {
  my $self = shift;

  # Look for an extra executable dependency cache file
  my $cachebase = $self->config->setting (qw/CACHE EXE_DEP/);
  my $incache   = find_file_in_path $cachebase, $self->{PATH}{CACHE};

  my $uptodate = 0;
  my @inlines  = ();

  # Read cache if it exists
  if ($incache and -r $incache) {
    my $incfg = Fcm::CfgFile->new (CONFIG => $self->config, SRC => $incache);
    $incfg->read_cfg;

    @inlines  = $incfg->lines;
    $uptodate = 1;
  }

  # Prepare output lines
  my $outcfg = Fcm::CfgFile->new (CONFIG => $self->config);
  $outcfg->add_line (COMMENT => 'EXE_DEP cache');

  # List of extra executable dependencies
  my %exe_dep   = %{ $self->config->setting ('EXE_DEP') };
  for my $target (sort keys %exe_dep) {
    $outcfg->add_line (
      LABEL => ($target ? 'OBJECTS__' . $target : 'OBJECTS'),
      VALUE => join (' ', sort keys %{ $exe_dep{$target} }),
    );
  }

  # List of BLOCKDATA dependencies
  my %blockdata = %{ $self->config->setting ('BLOCKDATA') };
  for my $target (sort keys %blockdata) {
    $outcfg->add_line (
      LABEL => ($target ? 'BLOCKDATA__' . $target : 'BLOCKDATA'),
      VALUE => join (' ', sort keys %{ $blockdata{$target} }),
    );
  }

  # List of EXE_NAME
  my %exe_name = %{ $self->config->setting ('EXE_NAME') };
  for my $target (sort keys %exe_name) {
    $outcfg->add_line (
      LABEL => 'EXE_NAME__' . $target,
      VALUE => $exe_name{$target},
    );
  }

  # Compare cache with current output
  my @outlines = $outcfg->lines ();

  $uptodate = 0 if @inlines != @outlines;

  if ($uptodate) {
    for my $i (0 .. $#outlines) {
      next unless $inlines[$i]->{LABEL} and $outlines[$i]->{LABEL};

      if ($inlines[$i]->{LABEL} ne $outlines[$i]->{LABEL} or
          $inlines[$i]->{VALUE} ne $outlines[$i]->{VALUE}) {
        $uptodate = 0;
        last;
      }
    }
  }

  # Update cache if it is out of date
  $outcfg->print_cfg (catfile ($self->{DIR}{CACHE}, $cachebase))
    unless $uptodate;

  # If out to date, set regenerate make rule flag to true
  $self->config->assign_setting (
    LABELS => [qw/REGEN_MAKERULE/],
    VALUE  => ! $uptodate,
  );

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_update_pp_info ();
#
# DESCRIPTION
#   This internal method compares the current set of pre-processor options
#   with that of the previous build using a "cache" file. If some
#   pre-processor options have changed, the method updates the cache file and
#   remove the "make" rules for the appropriate source packages.
# ------------------------------------------------------------------------------

sub _update_pp_info {
  my $self = shift;

  # Look for a PP option cache file
  my $cachebase = $self->config->setting (qw/CACHE PPOPTION/);
  my $incache   = find_file_in_path $cachebase, $self->{PATH}{CACHE};

  my @chgpp = ();
  my %newpp = %{ $self->{PP} };

  # Read config if exists, otherwise marked all current PP flags as "changed"
  if ($incache and -r $incache) {
    my $cfg = Fcm::CfgFile->new (CONFIG => $self->config, SRC => $incache);
    $cfg->read_cfg;

    my @lines   = $cfg->lines;
    my %oldpp = ();

    for my $line (@lines) {
      next unless $line->{LABEL};

      $oldpp{$line->{LABEL}} = $line->{VALUE};
    }

    # Compare new and old, mark as "changed" if changed or does not exist in old
    @chgpp = (grep {
      exists $oldpp{$_} ? $oldpp{$_} ne $newpp{$_} : 1;
    } keys %newpp);

    # Compare old and new, mark as "changed" if not exist in new
    push @chgpp, (grep {not exists $newpp{$_}} keys %oldpp);

  } else {
    @chgpp = keys %newpp;
  }

  if (@chgpp) {
    for my $name (@chgpp) {
      for my $package (values %{ $self->{PACKAGE} }) {
        next if $package->newpp;

        if (('PP__' . $package->name) =~ /^$name(?:__|$)/) {
          $package->current (1);
          $package->newpp   (1);
        }
      }
    }

    # Update the PP cache file if necessary
    my $cfg = Fcm::CfgFile->new (CONFIG => $self->config);

    for my $name (keys %newpp) {
      $cfg->add_line (LABEL => $name, VALUE => $newpp{$name});
    }
    $cfg->add_line unless $cfg->lines;

    $cfg->print_cfg (catfile ($self->{DIR}{CACHE}, $cachebase));
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_update_tool_info ();
#
# DESCRIPTION
#   This internal method compares the current set of build tools with that of
#   the previous build using a "cache" file. If some build tools have changed,
#   the method updates the cache file and (the time stamps of) dummy "flags"
#   files to denote changes in build tools from the previous build.
# ------------------------------------------------------------------------------

sub _update_tool_info {
  my $self = shift;

  # Look for a build tool cache file
  my $cachebase = $self->config->setting (qw/CACHE BLDTOOL/);
  my $incache   = find_file_in_path $cachebase, $self->{PATH}{CACHE};

  my @chgtool = ();
  my %newtool = %{ $self->config->setting ('TOOL') };

  # Read config if exists, otherwise marked all current tools as "changed"
  if ($incache and -r $incache) {
    my $cfg = Fcm::CfgFile->new (CONFIG => $self->config, SRC => $incache);
    $cfg->read_cfg;

    my @lines   = $cfg->lines;
    my %oldtool = ();

    for my $line (@lines) {
      next unless $line->{LABEL};

      $oldtool{$line->{LABEL}} = $line->{VALUE};
    }

    # Compare new and old, mark as "changed" if changed or does not exist in old
    @chgtool = (grep {
      exists $oldtool{$_} ? $oldtool{$_} ne $newtool{$_} : 1;
    } keys %newtool);

    # Compare old and new, mark as "changed" if not exist in new
    push @chgtool, (grep {not exists $newtool{$_}} keys %oldtool);

  } else {
    @chgtool = keys %newtool;
  }

  if (@chgtool) {
    # Update the time stamps of dummy files for changed tools
    $self->_create_build_dir ('FLAGS');

    my $ext = $self->config->setting (qw/OUTFILE_EXT FLAGS/);
    for my $name (@chgtool) {
      my $file = catfile $self->{DIR}{FLAGS}, $name . $ext;

      # Create/touch the file
      touch_file $file or croak 'Unable to update: ', $file, ', abort';

      print 'Updated: ', $file, "\n" if $self->config->verbose > 2;
    }

    # Update the build tool cache file if necessary
    my $cfg = Fcm::CfgFile->new (CONFIG => $self->config);

    for my $name (keys %newtool) {
      $cfg->add_line (LABEL => $name, VALUE => $newtool{$name});
    }

    $cfg->print_cfg (catfile ($self->{PATH}{CACHE}->[0], $cachebase));
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_pre_process ();
#
# DESCRIPTION
#   This internal method obtains a list of source files that require
#   pre-processing in the source packages of this build, and attempts to
#   pre-process them. The method returns 1 on success.
# ------------------------------------------------------------------------------

sub _pre_process {
  my $self = shift;

  # Go through source packages/files to see if PP is required
  my @srcfiles = ();
  for my $package (values %{ $self->{PACKAGE} }) {
    next unless $package->requirepp;

    $package->scan_dependency (HEADER_ONLY => 1);

    push @srcfiles, grep ({$_->is_type_or (qw/FPP C/)} $package->srcfile);
  }

  return 1 unless @srcfiles;

  my %task     = ();
  my $flagsext = $self->config->setting (qw/OUTFILE_EXT FLAGS/);
  my $pdoneext = $self->config->setting (qw/OUTFILE_EXT PDONE/);

  # Set up tasks for each source file
  for my $srcfile (@srcfiles) {
    my $command  = $srcfile->is_type ('FPP') ? 'FPP' : 'CPP';
    my @pck_list = $srcfile->get_package_list;
    my @pknames  = split '__', pop (@pck_list);

    # Set up a PP build task for each source file
    my $target    = $srcfile->base . $pdoneext;
    my $ppkeyname = join ('__', ($command . 'KEYS' , @pknames)) . $flagsext;
    my $flagsname = join ('__', ($command . 'FLAGS', @pknames)) . $flagsext;

    # Issue warning for duplicated tasks
    if (exists $task{$target}) {
      w_report 'Warning: ', $target, ': unable to create task for: ',
               $srcfile->src, ': task already exists for: ',
               $task{$target}->srcfile->src;
      next;
    }

    $task{$target} = Fcm::BuildTask->new (
      CONFIG     => $self->config,
      TARGET     => $target,
      TARGETPATH => $self->{PATH}{DONE},
      SRCFILE    => $srcfile,
      DEPENDENCY => [$ppkeyname, $flagsname, ($srcfile->dep ('H'))],
      ACTIONTYPE => 'PP',
    );

    # Set up update ppkeys/flags build tasks for each source file/package
    for my $i (0 .. $#pknames) {
      my $name  = join '__', @pknames [0 .. $i];     # package name
      my $dname = join '__', @pknames [0 .. $i - 1]; # dependent package name

      for my $flag (qw/KEYS FLAGS/) {
        my $fullflag = $command . $flag;
        my $target   = join '__', ($fullflag, $name);
        my $depend   = $dname ? join '__', ($fullflag, $dname) : $fullflag;

        $target .= $flagsext;
        $depend .= $flagsext;

        next if exists $task{$target};

        $task{$target} = Fcm::BuildTask->new (
          CONFIG     => $self->config,
          TARGET     => $target,
          TARGETPATH => $self->{PATH}{FLAGS},
          DEPENDENCY => [$depend],
          ACTIONTYPE => 'UPDATE',
        );
      }
    }
  }

  # Set up update global ppkeys/flags build tasks
  for my $command (qw/CPP FPP/) {
    for my $flag ('', qw/KEYS FLAGS/) {
      my $target = $command . $flag . $flagsext;

      $task{$target} = Fcm::BuildTask->new (
        CONFIG     => $self->config,
        TARGET     => $target,
        TARGETPATH => $self->{PATH}{FLAGS},
        ACTIONTYPE => 'UPDATE',
      );
    }
  }

  # Set up build tasks to copy all header files
  for my $package (values %{ $self->{PACKAGE} }) {
    my @files = grep {$_->is_type (qw/CPP INCLUDE/)} $package->srcfile;

    # Author's note: may also want to issue warning for duplicated tasks

    for my $file (@files) {
      $task{$file->base} = Fcm::BuildTask->new (
        CONFIG     => $self->config,
        TARGET     => $file->base,
        TARGETPATH => $self->{PATH}{INC},
        SRCFILE    => $file,
        DEPENDENCY => [$file->dep ('H')],
        ACTIONTYPE => 'COPY',
      );
    }
  }

  # Build all PP tasks
  my $count = 0;

  for my $task (values %task) {
    next unless $task->actiontype eq 'PP';

    my $rc = $task->action (TASKLIST => \%task);
    $count++ if $rc;
  }

  print 'Number of pre-processed files: ', $count, "\n"
    if $self->config->verbose and $count;

  # Change path and file type of pre-processed source files
  for my $task (values %task) {
    next unless $task->actiontype eq 'PP';

    # Remove header dependencies from source file
    my %dep = $task->srcfile->dep ();
    for my $key (keys %dep) {
      delete $dep{$key} if $dep{$key} eq 'H';
    }
    $task->srcfile->dep (\%dep);
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_generate_f9x_interface ();
#
# DESCRIPTION
#   This internal method obtains a list of Fortran 9X source files in the
#   source packages of this build, and attempts to generate an interface block
#   file for each of the Fortran 9X source files. The method returns 1 on
#   success.
# ------------------------------------------------------------------------------

sub _generate_f9x_interface {
  my $self = shift;

  # Go through source packages/files for Fortran 9x source files with
  # standalone subroutines or functions
  my @srcfiles = ();
  for my $package (values %{ $self->{PACKAGE} }) {
    next unless $package->current;

    push @srcfiles, grep {
      $_->is_type_or (qw/FORTRAN9X FPP9X/) and
      uc ($_->select_tool ('GENINTERFACE')) ne 'NONE' and
      not $_->is_type_or (qw/PROGRAM MODULE INCLUDE/)
    } $package->srcfile;
  }

  my $flagsext = $self->config->setting (qw/OUTFILE_EXT FLAGS/);
  my $pdoneext = $self->config->setting (qw/OUTFILE_EXT PDONE/);

  # Set up build task to generate interface files for all selected Fortran 9x
  # sources
  my %task         = ();
  for my $srcfile (@srcfiles) {
    my $target  = $srcfile->interfacebase . $pdoneext;
    my @pknames = split '__', ($srcfile->get_package_list)[-1];
    my $flag    = join ('__', ('GENINTERFACE', @pknames)) . $flagsext;

    $task{$target} = Fcm::BuildTask->new (
      CONFIG     => $self->config,
      TARGET     => $target,
      TARGETPATH => $self->{PATH}{DONE},
      SRCFILE    => $srcfile,
      DEPENDENCY => [$flag],
      ACTIONTYPE => 'GENINTERFACE',
    );

    # Set up build tasks for each source file/package flags file for interface
    # generator tool
    for my $i (0 .. $#pknames) {
      my $name   = join '__', @pknames [0 .. $i];     # package name
      my $dname  = join '__', @pknames [0 .. $i - 1]; # dependent package name

      my $target = join '__', ('GENINTERFACE', $name);
      my $depend = $dname ? join '__', ('GENINTERFACE', $dname) : 'GENINTERFACE';

      $target .= $flagsext;
      $depend .= $flagsext;

      next if exists $task{$target};

      $task{$target} = Fcm::BuildTask->new (
        CONFIG     => $self->config,
        TARGET     => $target,
        TARGETPATH => $self->{PATH}{FLAGS},
        DEPENDENCY => [$depend],
        ACTIONTYPE => 'UPDATE',
      );
    }
  }

  # Set up build task to update the flags file for interface generator tool
  {
    my $target     = 'GENINTERFACE' . $flagsext;
    $task{$target} = Fcm::BuildTask->new (
      CONFIG     => $self->config,
      TARGET     => $target,
      TARGETPATH => $self->{PATH}{FLAGS},
      ACTIONTYPE => 'UPDATE',
    );
  }

  my $count = 0;

  # Performs task
  for my $task (values %task) {
    next unless $task->actiontype eq 'GENINTERFACE';

    my $rc = $task->action (TASKLIST => \%task);
    $count++ if $rc;
  }

  print 'Number of generated interfaces: ', $count, "\n"
    if $self->config->verbose and $count;

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_scan_dependency ();
#
# DESCRIPTION
#   This internal method goes through each source package to obtain dependency
#   information from their source files. It returns 1 on success.
# ------------------------------------------------------------------------------

sub _scan_dependency {
  my $self = shift;

  # Go through source packages/files
  my $count = 0;

  for my $package (values %{ $self->{PACKAGE} }) {
    my $rc = $package->scan_dependency;
    $count++ if $rc;
  }

  print 'Scanned files in ', $count, ' package(s) for dependency', "\n"
    if $self->config->verbose and $count;

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_set_targets ();
#
# DESCRIPTION
#   This internal method determines the default targets to be built.
# ------------------------------------------------------------------------------

sub _set_targets {
  my $self = shift;

  # Targets of the build
  if (not @{ $self->{TARGET} }) {
    # Build targets not specified by user, default to building all main programs
    my @programs = ();

    # Get all main programs from all packages
    for my $package (values %{ $self->{PACKAGE} }) {
      my @srcfiles = grep {$_->exebase} $package->srcfile;

      for (@srcfiles) {
        push @programs, $_->exebase;
      }
    }

    @programs = sort (@programs);

    if (@programs) {
      # Build main programs, if there are any
      @{ $self->{TARGET} } = @programs;

    } else {
      # No main program in source tree, build the default library
      @{ $self->{TARGET} } = (
        'lib' . $self->{LIB}{''} . $self->config->setting (qw/OUTFILE_EXT LIB/),
      );
    }
  }

  return @{ $self->{TARGET} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_write_make_rules ();
#
# DESCRIPTION
#   This internal method writes the included make rules if necessary.
# ------------------------------------------------------------------------------

sub _write_make_rules {
  my $self   = shift;
  my $return = 1;

  # Get list of all packages
  my %allpcks = $self->allpcks;

  # Get list of types that cannot have duplicated targets
  my @no_duplicated_target_types = split (
    /,/, $self->config->setting ('NO_DUPLICATED_TARGET_TYPE'),
  );

  my $unusual = 0;
  my $count   = 0;
  my %targets;
  for my $name (sort keys %allpcks) {
    # Check whether package is an actual package of the build
    my $package  = exists $self->{PACKAGE}{$name} ? $self->{PACKAGE}{$name} : '';

    # Regenerate make rules if this flag is set to true
    my $regen_mk;

    # Register non-word package name
    if (not $name =~ /^\w*$/) {
      $self->config->assign_setting (
        LABELS => ['FCM_PCK_OBJECTS', $name],
        VALUE  => 'FCM_PCK_OBJECTS' . $unusual++,
      );

      # Set regenerate make rule flag to true
      $regen_mk = 1;
    }

    # Cycle loop if not an actual package of the build
    next unless $package;

    # Check whether make rule for each source package needs updating. Yes if:
    # 1. package name contains non-word characters
    # 2. the config setting REGEN_MAKERULE is set to true
    # 3. package is current and the make rule for the package is out of date
    $regen_mk = (
      $regen_mk or
      $self->config->setting ('REGEN_MAKERULE') or
      ($package->current and not $package->makerule_uptodate)
    );

    # Update make rule for source package, if necessary
    $count += $package->write_makerule () if $regen_mk;

    # Check for duplicated rules
    for my $srcfile (sort $package->srcfile) {
      next unless $srcfile->type;

      my %rules = $srcfile->required_rules;

      for my $key (sort keys %rules) {
        if (exists $targets{$key}) {
          # Duplicated target: warning for most file types
          my $status = 'WARNING';

          # Duplicated target: error for the following file types
          if (@no_duplicated_target_types and
              $srcfile->is_type_or (@no_duplicated_target_types) and
              $targets{$key}->is_type_or (@no_duplicated_target_types)) {
            $status = 'ERROR';
            $return = 0;
          }

          # Report the warning/error
          w_report $status, ': ', $key, ': duplicated targets for building:';
          w_report '       ', $targets{$key}->src;
          w_report '       ', $srcfile->src;

        } else {
          $targets{$key} = $srcfile;
        }
      }
    }
  }

  # Diagnostic
  print 'Updated make rules for ', $count, ' package(s).', "\n"
    if $count and $self->config->verbose;

  return $return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_write_makefile ();
#
# DESCRIPTION
#   This internal method writes the "Makefile" for this build.
# ------------------------------------------------------------------------------

sub _write_makefile {
  my $self = shift;

  # Makefile header
  # ----------------------------------------------------------------------------
  my $makefile = '# Automatic Makefile' . "\n\n";

  # Name of the build
  $makefile .= 'FCM_BUILD_NAME = ' . $self->{NAME} . "\n" if $self->{NAME};
  
  # Location of FCM config file
  $makefile .= 'FCM_BLD_CFG = ' . $self->cfg->src . "\n";

  # Targets of the build
  $makefile .= 'FCM_BLD_TARGETS = ' . join (' ', ($self->_set_targets)) . "\n\n";

  # Perl library
  # ----------------------------------------------------------------------------
  {
    my $libdir  = dirname (dirname ($INC{'Fcm/Build.pm'}));
    my @libpath = split /:/, ($ENV{PERL5LIB} ? $ENV{PERL5LIB} : '');

    if (not grep (m/$libdir/, @libpath)) {
      $makefile .= 'export PERL5LIB := ' . $libdir;
      $makefile .= ':$(PERL5LIB)' if exists $ENV{PERL5LIB};
      $makefile .= "\n\n";
    }
  }

  # Build directories
  # ----------------------------------------------------------------------------
  my @keys    = ('ROOT', sort grep {$_ ne 'ROOT'} keys (%{ $self->{DIR} }));
  my $rootdir = $self->{DIR}{ROOT};

  # Build sub-directories
  for my $name (@keys) {
    my $dir = $self->{DIR}{$name};
    $dir    =~ s/^$rootdir/\$(FCM_ROOTDIR)/ unless $name eq 'ROOT';

    $makefile .= 'export FCM_' . $name . 'DIR = ' . $dir . "\n";
  }

  $makefile .= "\n";

  # Build sub-directory paths
  for my $name (@keys) {
    my @path = @{ $self->{PATH}{$name} };
    shift @path;

    $makefile .= 'export FCM_' . $name . 'PATH = ' .
                 join (':', ('$(FCM_' . $name . 'DIR)', @path)) . "\n";
  }

  $makefile .= "\n";

  # Build tools
  # ----------------------------------------------------------------------------
  # List of build tools
  my $tool          = $self->config->setting ('TOOL');

  # List of tools local to FCM, (will not be exported)
  my %localtool     = map {($_, 1)} split (    # map into a hash table
    /,/, $self->config->setting ('LOCALTOOL'), # split comma separated list
  );

  # Export required tools
  my $unusual_count = 0;
  for my $name (sort keys %$tool) {
    # Ignore local tools
    my $topname = (split (/__/, $name))[0];
    next if exists $localtool{$topname};

    if ($name =~ /^\w+$/) {
      # Tools with normal name, just export it as an environment variable
      $makefile .= 'export ' . $name . ' = ' . $tool->{$name} . "\n";

    } else {
      # Tools with unusual characters, export using a label/value pair
      $makefile .= 'export FCM_UNUSUAL_TOOL_LABEL' . $unusual_count . ' = ' .
                   $name . "\n";
      $makefile .= 'export FCM_UNUSUAL_TOOL_VALUE' . $unusual_count . ' = ' .
                   $tool->{$name} . "\n";
      $unusual_count++;
    }
  }

  $makefile .= "\n";

  # Verbose mode
  # ----------------------------------------------------------------------------
  $makefile .= 'export FCM_VERBOSE ?= ' . $self->config->verbose . "\n\n";

  # VPATH
  # ----------------------------------------------------------------------------
  # $name is internal name of build sub-directories
  # $type is the type of files
  for my $name (sort keys %{ $self->config->setting ('VPATH') }) {
    my @types = split /,/, $self->config->setting ('VPATH', $name);

    for my $type (sort @types) {
      # If $type is EMPTY, it is a file with no file name extension
      if (uc ($type) eq 'EMPTY') {
        $makefile .= 'vpath % $(FCM_' . $name . 'PATH)' . "\n";

      } elsif ($type =~ s/^(in|out)://i) {
        if (uc ($1) eq 'IN') {
          # If $type begins with IN:<type>, it is a list of file extensions that
          # can be found under the INFILE_EXT hash in the configuration setting,
          # with <type> matching a keyword in the values of the hash.
          my %infile_ext = %{ $self->config->setting ('INFILE_EXT') };

          for my $ext (sort keys %infile_ext) {
            $makefile .= 'vpath %.' . $ext . ' $(FCM_' . $name . 'PATH)' . "\n"
              if grep {$_ eq $type} split /::/, $infile_ext{$ext};
          }

        } else {
          # If $type begins with OUT:<type>, it is the value of a hash element
          # in the OUTFILE_EXT hash in the configuration setting, with <type>
          # matching the key.
          my $ext    = $self->config->setting ('OUTFILE_EXT', $type);
          $makefile .= 'vpath %' . $ext . ' $(FCM_' . $name . 'PATH)' . "\n";
        }

      } else {
        # Otherwise, $type is a VPATH pattern recognised by "make".
        $makefile .= 'vpath ' . $type . ' $(FCM_' . $name . 'PATH)' . "\n";
      }
    }
  }

  # VPATH for dummy files
  $makefile .= 'vpath %.dummy $(FCM_DONEDIR)' . "\n";
  $makefile .= "\n";

  # Default targets
  # ----------------------------------------------------------------------------
  $makefile .= '.PHONY : all clean' . "\n\n";
  $makefile .= 'all : $(FCM_BLD_TARGETS)' . "\n\n";
  $makefile .= 'clean : ' . "\n";
  $makefile .= "\t" . 'rm -rf';
  for my $dir (qw/BIN LIB OBJ DONE/) {
    $makefile .= ' $(FCM_' . $dir . 'DIR)' if exists $self->{DIR}{$dir};
  }
  $makefile .= "\n";
  $makefile .= "\t" . 'rm -f lib__*' .
               $self->config->setting (qw/OUTFILE_EXT LIB/) .
               ' *' . $self->config->setting (qw/OUTFILE_EXT OBJ/) . "\n";
  $makefile .= "\n";

  # Targets for copy dummy
  $makefile .= $self->config->setting (qw/MISC CPDUMMY/) . ' :' . "\n";
  $makefile .= "\t" . 'touch $@' . "\n\n";

  # Targets for all (non-main-program) objects and libraries
  # ----------------------------------------------------------------------------
  my %allpcks = $self->allpcks;
  for my $key (reverse sort keys %allpcks) {
    # Objects variable
    my $var;
    if ($self->config->setting ('FCM_PCK_OBJECTS', $key)) {
      # Package name contains unusual characters, use predefined variable
      $var = $self->config->setting ('FCM_PCK_OBJECTS', $key);

    } else {
      # Normal package name, prefix the package name with "OBJECTS__"
      # Top level package, simply set to "OBJECTS"
      $var = $key ? join ('__', ('OBJECTS', $key)) : 'OBJECTS';
    }

    # Export top level OBJECTS variable
    # but keep sub-package OBJECTS variables local to the Makefile
    $makefile .= ($var eq 'OBJECTS' ? 'export ' : '') . $var . ' =';

    # Add objects from children
    if (@{ $allpcks{$key} }) {
      # List of sub-packages of current package
      my @deps   = map {
        if ($self->config->setting ('FCM_PCK_OBJECTS', $_)) {
          # Package name contains unusual characters, use predefined variable
          '$(' . $self->config->setting ('FCM_PCK_OBJECTS', $_) . ')';

        } else {
          # Normal package name, prefix the package name with "OBJECTS__"
          '$(OBJECTS__' . $_ . ')';
        }
      } @{ $allpcks{$key} };

      $makefile .= ' ' . join (' ', sort @deps);
    }

    # Add its own objects
    if (exists $self->{PACKAGE}{$key}) {
      # List of source files in the current package
      my @files = sort {$a->base cmp $b->base} $self->{PACKAGE}{$key}->srcfile;

      for my $file (@files) {
        # Consider compilable source files only
        next unless $file->objbase;

        # Ignore main programs and Fortran BLOCKDATA program units
        next if $file->is_type_or (qw/PROGRAM BLOCKDATA/);

        # Add to object list
        $makefile .= ' ' . $file->objbase;
      }
    }

    $makefile .= "\n\n";

    # Library target
    my $lib = exists ($self->{LIB}{$key}) ? $self->{LIB}{$key} : $key;
    $lib    = 'lib' . $lib . $self->config->setting (qw/OUTFILE_EXT LIB/);

    $makefile .= $lib . ' : $(' . $var . ')' . "\n";
    $makefile .= "\t" . 'fcm_internal archive $@ $(^F)' . "\n\n";
  }

  # Targets for top level and package flags files and dummy dependencies
  my %src_tool   = %{ $self->config->setting ('SRC_TOOL') };
  my %flags_tool = (LD => '', LDFLAGS => '');

  for my $key (keys %src_tool) {
    $flags_tool{$src_tool{$key}{FLAGS}} = $src_tool{$key}{COMPILER}
      if exists $src_tool{$key}{FLAGS};

    $flags_tool{$src_tool{$key}{PPKEYS}} = ''
      if exists $src_tool{$key}{PPKEYS};
  }

  my $ext = $self->config->setting (qw/OUTFILE_EXT FLAGS/);
  for my $name (sort keys %flags_tool) {
    # Flags files for tool command
    if ($flags_tool{$name}) {
      $makefile .= $flags_tool{$name} . $ext . ' :' . "\n";
      $makefile .= "\t" . 'touch ' . catfile ('$(FCM_FLAGSDIR)', '$@') . "\n\n";
    }

    # Top level flags files
    $makefile .= $name . $ext . ' :';
    $makefile .= ' ' . $flags_tool{$name} . $ext if $flags_tool{$name};
    $makefile .= "\n\t" . 'touch ' . catfile ('$(FCM_FLAGSDIR)', '$@') . "\n\n";

    # Package level flags files
    for my $key (sort keys %allpcks) {
      next unless @{ $allpcks{$key} }; # ignore packages without children

      my $depend  = $key ? join '__', ($name, $key) : $name;
      my @targets = sort map {$name . '__' . $_ . $ext} @{ $allpcks{$key} };

      $makefile .= join (' ', @targets) . ' : ' . $depend . $ext . "\n";
      $makefile .= "\t" . 'touch ' . catfile ('$(FCM_FLAGSDIR)', '$@') .
                   "\n\n";
    }
  }

  # Include source package make rules
  # ----------------------------------------------------------------------------
  for my $package (sort {$a->name cmp $b->name} values %{ $self->{PACKAGE} }) {
    my $mkbase = $package->name . $self->config->setting (qw/OUTFILE_EXT MK/);
    my $mkfile = find_file_in_path ($mkbase, $self->{PATH}{BLD});

    if ($mkfile) {
      if (index ($mkfile, $self->{DIR}{BLD}) == 0) {
        $mkfile = catfile '$(FCM_BLDDIR)',
                  substr ($mkfile, length ($self->{DIR}{BLD}) + 1);

      } elsif (index ($mkfile, $self->{DIR}{ROOT}) == 0) {
        $mkfile = catfile '$(FCM_ROOTDIR)',
                  substr ($mkfile, length ($self->{DIR}{ROOT}) + 1);
      }

      $makefile .= 'include ' . $mkfile . "\n";

    } else {
      my $pck = join ('::', split (/__/, $package->name));
      w_report 'Warning: no make rule file for source package: ', $pck;
    }
  }

  $makefile .= "\n" . '# EOF' . "\n";

  # Print Makefile
  # ----------------------------------------------------------------------------
  $self->_create_build_dir ('BLD');
  my $out = catfile (
    $self->{DIR}{BLD}, $self->config->setting (qw/MISC MAKEFILE/),
  );

  # Check whether an old file exists, if so compare current contents with it
  my $old = '';

  if (-r $out) {
    open OLD, '<', $out or croak 'Cannot open "', $out, '" (', $!, '), abort';
    my @lines = readline 'OLD';
    close OLD;

    $old = join ('', @lines);
  }

  # Update Makefile if changed
  if ($old ne $makefile) {
    open OUT, '>', $out or croak 'Cannot open "', $out, '" (', $!, '), abort';
    print OUT $makefile;
    close OUT or croak 'Cannot close "', $out, '" (', $!, '), abort';

    print 'Updated Makefile: ', $out, "\n" if $self->config->verbose;
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_invoke_make (
#     TARGETS => \@targets,
#     JOBS    => $jobs,
#     ARCHIVE => $archive,
#   );
#
# DESCRIPTION
#   This internal method invokes the "make" command to make the build.
#
# ARGUMENTS
#   TARGETS - Specify targets to be built. If set, these targets will be built
#             instead of the ones specified in the build configuration file.
#   JOBS    - Specify number of jobs that can be handled by "make". If set,
#             the value must be a natural integer. If not set, the default
#             value is 1 (i.e. run "make" in serial mode).
#   ARCHIVE - If set to "true", invoke the "archive" mode. Most build files and
#             directories created by this build will be archived using the
#             "tar" command. If not set, the default is not to invoke the
#             "archive" mode.
# ------------------------------------------------------------------------------

sub _invoke_make {
  my $self = shift;
  my %args = @_;

  # Build the make command from the specified targets
  my @targets  = exists $args{TARGETS} ? @{ $args{TARGETS} } : qw/all/;
  my $jobs     = exists $args{JOBS}    ? $args{JOBS}         : 1;
  my $archive  = exists $args{ARCHIVE} ? $args{ARCHIVE}      : 0;
  my $verbose  = $self->config->verbose;

  # Create the required build directories
  for my $dir (qw/BIN DONE ETC INC FLAGS LIB OBJ TMP/) {
    $self->_create_build_dir ($dir);
  }  

  my @commands = ();
  my @make_cmd = ($self->config->setting (qw/TOOL MAKE/));
  push @make_cmd, split (/\s+/, $self->config->setting (qw/TOOL MAKEFLAGS/));
  push @make_cmd, $self->config->setting (qw/TOOL MAKE_SILENT/)
    unless $verbose > 2;

  if ($jobs > 1) { # multi-process "make"
    my $make_job = $self->config->setting (qw/TOOL MAKE_JOB/);

    # Setup the "make" commands for each target
    while (my $target = shift @targets) {
      if ($target eq 'clean') { # Do not run "clean" in parallel
        push @commands, [@make_cmd, $target];

      } else {
        push @commands, [@make_cmd, $make_job, $jobs, $target];
      }
    }

  } else { # single process "make"

    # Setup the "make" command
    push @commands, [@make_cmd, @targets];

  }
  
  # Run the make command
  my $rc  = 0;
  my $cwd = cwd;
  print 'cd ', $self->{DIR}{BLD}, "\n" if $verbose > 2;
  chdir $self->{DIR}{BLD};
  while (my $cmd = shift @commands) {
    $| = 1; # flush STDOUT before running "make"
    print timestamp_command (&get_command_string ($cmd)) if $verbose > 2;
    $| = 0;
    &run_command ($cmd, ERROR => 'warn', RC => \$rc);
    print timestamp_command (&get_command_string ($cmd), 'End') if $verbose > 2;
    last if $rc;
  }
  print 'cd ', $cwd, "\n" if $verbose > 2;
  chdir $cwd;

  return $rc ? undef : 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = $self->_remove_empty_dirs ();
#
# DESCRIPTION
#   This internal method removes empty build directories.
# ------------------------------------------------------------------------------

sub _remove_empty_dirs {
  my $self = shift;

  for my $name (qw/BIN CACHE DONE ETC FLAGS INC LIB PPSRC OBJ TMP/) {
    opendir DIR, $self->{DIR}{$name};
    my @files = readdir DIR;
    @files    = grep !/^\.\.?$/, @files;
    closedir DIR;

    if (not @files) {
      print 'Remove directory: ', $self->{DIR}{$name}, "\n"
        if $self->config->verbose > 1;
      rmdir $self->{DIR}{$name};
    }
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = $self->_tar_build_dirs ();
#
# DESCRIPTION
#   This internal method creates TAR archives for selected build directories.
# ------------------------------------------------------------------------------

sub _tar_build_dirs {
  my $self = shift;

  # Create TAR archives if necessary
  my $cwd = cwd;

  my $tar      = $self->config->setting (qw/OUTFILE_EXT TAR/);
  my @tar_dirs = split /,/, $self->config->setting (qw/TAR_DIRS/);
  my $verbose  = $self->config->verbose;

  for my $name (@tar_dirs) {
    my $dir = $self->{DIR}{$name};

    if (-d $dir) {
      my $base = basename ($dir);
      print 'cd ', dirname ($dir), "\n" if $verbose > 2;
      chdir dirname ($dir);

      my $rc = &run_command (
        [qw/tar -c -f/, $base . $tar, $base],
        PRINT => $verbose > 1, ERROR => 'warn',
      );

      &run_command ([qw/rm -rf/, $base], PRINT => $verbose > 1) if not $rc;
    }
  }

  print 'cd ', $cwd, "\n" if $verbose > 2;
  chdir $cwd;

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = $self->_create_runenv_script ();
#
# DESCRIPTION
#   This internal method creates the runtime environment script if necessary.
# ------------------------------------------------------------------------------

sub _create_runenv_script {
  my $self = shift;

  # More diagnostic on how to use the build
  my @bin_dirs = grep {-d} @{ $self->{PATH}{BIN} };
  my $etc_dir  = -d $self->{DIR}{ETC} ? $self->{DIR}{ETC} : undef;

  if (@bin_dirs or $etc_dir) {
    # Create a runtime environment script if necessary
    my $run_env_sh_base = $self->config->setting (qw/MISC RUN_ENV_SH/);
    my $run_env_sh      = catfile $self->{DIR}{ROOT}, $run_env_sh_base;

    open FILE, '>', $run_env_sh
      or croak $run_env_sh, ': cannot open (', $!, '), abort';
    print FILE '#!/usr/bin/ksh', "\n";
    print FILE 'export PATH=', join (':', @bin_dirs), ':$PATH', "\n"
      if @bin_dirs;
    print FILE 'export FCM_ETCDIR=', $self->{DIR}{ETC}, "\n" if $etc_dir;
    close FILE or croak $run_env_sh, ': cannot close (', $!, '), abort';

    # Create symbolic link in bin/ sub-directory for backward compatibility
    if (-d $self->{DIR}{BIN}) {
      my $file = catfile ($self->{DIR}{BIN}, $run_env_sh_base);
      
      # Remove old link if necessary
      unlink $file if -l $file and readlink ($file) ne $run_env_sh;

      # Create the new link
      symlink $run_env_sh, $file if not -l $file;
    }

    # Information on the location/usage of the runtime environment script
    if ($self->config->verbose > 1 and $run_env_sh) {
      print '# ', '-' x 78, "\n";
      print '# To use this build, source the following shell script:', "\n";
      print '. ', $run_env_sh, "\n";
      print '# ', '-' x 78, "\n";
    }
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_create_lib_excl_dep ();
#
# DESCRIPTION
#   This internal method creates a set of exclude dependency configurations for
#   libraries of this build.
# ------------------------------------------------------------------------------

sub _create_lib_excl_dep {
  my $self = shift;

  if (-d $self->{DIR}{LIB}) {
    $self->_create_build_dir ('ETC');
    
    my %allpcks  = $self->allpcks;
    my $cfgext   = $self->config->setting (qw/OUTFILE_EXT CFG/);
    my %cfglabel = %{ $self->config->setting ('CFG_LABEL') };

    for my $key (reverse sort keys %allpcks) {
      my $outcfg = Fcm::CfgFile->new (CONFIG => $self->config);

      # Include configurations from sub-packages
      for my $subpck (@{ $allpcks{$key} }) {
        my $base = 'lib' . $subpck . $cfgext;
        ($base = $self->{LIB}{$subpck}) =~ s/\.\w+$/$cfgext/
          if exists ($self->{LIB}{$subpck});
        my $file = catfile ('$HERE', $base);

        $outcfg->add_line (LABEL => $cfglabel{INC}, VALUE => $file)
          if -r catfile ($self->{DIR}{ETC}, $base);;
      }

      # Exclude dependency for source files in current package
      if (exists $self->{PACKAGE}{$key}) {
        my @srcfiles = $self->{PACKAGE}{$key}->srcfile;

        for my $srcfile (@srcfiles) {
          if ($srcfile->is_type ('INCLUDE')) {
            if ($srcfile->is_type ('CPP')) {
              $outcfg->add_line (
                LABEL => $cfglabel{EXCL_DEP},
                VALUE => 'H::' . $srcfile->base,
              );

            } elsif ($srcfile->is_type ('INTERFACE')) {
              $outcfg->add_line (
                LABEL => $cfglabel{EXCL_DEP},
                VALUE => 'INTERFACE::' . $srcfile->base,
              );

            } else {
              $outcfg->add_line (
                LABEL => $cfglabel{EXCL_DEP},
                VALUE => 'INC::' . $srcfile->base,
              );
            }

          } elsif ($srcfile->is_type ('SOURCE')) {
            next if $srcfile->is_type_or (qw/PROGRAM BLOCKDATA/);

            if ($srcfile->is_type ('FORTRAN')) {
              if ($srcfile->is_type (qw/FORTRAN MODULE/)) {
                $outcfg->add_line (
                  LABEL => $cfglabel{EXCL_DEP},
                  VALUE => 'USE::' . $srcfile->root,
                );

              } else {
                $outcfg->add_line (
                  LABEL => $cfglabel{EXCL_DEP},
                  VALUE => 'INTERFACE::' . $srcfile->interfacebase,
                ) if $srcfile->interfacebase;

                $outcfg->add_line (
                  LABEL => $cfglabel{EXCL_DEP},
                  VALUE => 'OBJ::' . $srcfile->root,
                );
              }

            } else {
              $outcfg->add_line (
                LABEL => $cfglabel{EXCL_DEP},
                VALUE => 'OBJ::' . $srcfile->root,
              );
            }
          }
        }
      }

      # Name of configuration file, follows the name of library
      my $outbase ='lib' . $key . $cfgext;
      ($outbase = $self->{LIB}{$key}) =~ s/\.\w+$/$cfgext/
        if exists ($self->{LIB}{$key});
      my $outfile = catfile ($self->{DIR}{ETC}, $outbase);

      # Write to configuration file
      $outcfg->print_cfg ($outfile);
    }

    # Information on the location/usage of the exclude dependency configurations
    if ($self->config->verbose > 1) {
      my $etcdir = $self->{DIR}{ETC};
      print '# ', '-' x 78, "\n";
      print <<EOF;
# To use a library archive of this build in another FCM build, you need to
# include in the new build configuration the corresponding configuration file
# that has the relevant exclude dependency information. These configurations
# files can be found in $etcdir.
EOF
      print '# ', '-' x 78, "\n";
    }
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_create_build_dir ();
#   $self->_create_build_dir ($label);
#
# DESCRIPTION
#   This internal method creates a build directory. If $label is specified,
#   the method will attempt to create a named sub-directory according to
#   $label. Otherwise, the method attempts to create the build root
#   directory. Returns the name of the directory if it is created successfully
#   or if it already exists.
# ------------------------------------------------------------------------------

sub _create_build_dir {
  my $self  = shift;
  my $label = $_[0] ? uc $_[0] : 'ROOT';

  my $dir = undef;

  # Make sure the variable is set
  if ($self->{DIR}{$label}) {
    $dir = $self->{DIR}{$label};

    # Expand relative path if necessary
    $dir = catfile $self->{DIR}{ROOT}, $dir if $dir =~ /^\w/;

  } else {
    if ($label eq 'ROOT') {
      w_report 'Error: build root directory not set.';
      return;

    } elsif ($self->config->setting ('DIR', $label)) {
      $dir = catfile $self->{DIR}{ROOT}, $self->config->setting ('DIR', $label);

    } else {
      carp 'Directory label "', $label, '" not recognised';
      return undef;
    }
  }

  # Set up the bld directory, if required
  if (not -d $dir) {
    print 'Make directory: ', $dir, "\n" if $self->config->verbose > 1;
    mkpath $dir or croak 'Cannot create directory "', $dir, '"';
  }

  $self->{DIR}{$label} = $dir unless $self->{DIR}{$label};

  return $dir;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_get_inherited_paths ($name);
#
# DESCRIPTION
#   This recursive internal method returns a list containing the search path
#   for a build directory named by the internal label $name. (Please note that
#   a build directory will only be placed into the search path if the
#   directory exists.)
# ------------------------------------------------------------------------------

sub _get_inherited_paths {
  my $self = shift;
  my $name = shift;

  return () unless $name and exists $self->{DIR}{$name};

  my @path = ();

  # Recursively inherit the search path for a this type of build directory
  for my $use (@{ $self->{USE} }) {
    my @cur_path = $use->_get_inherited_paths ($name);
    unshift @path, @cur_path;
  }

  # Place the path of the current build in the front
  unshift @path, $self->{DIR}{$name};

  return @path;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_get_inherited_srcdirs ($name);
#
# DESCRIPTION
#   This recursive internal method returns a list containing the search path
#   for a source directory named by the internal package label $name. (Please
#   note that a source directory will only be placed into the search path if
#   the directory exists.)
# ------------------------------------------------------------------------------

sub _get_inherited_srcdirs {
  my $self = shift;
  my $name = shift;

  return () unless $name;

  my @path = ();

  # Recursively inherit the search path for this source directory
  my $key = 'SRCDIR__' . $name;
  if ($self->_inherit_ok ($key)) {
    for my $use (@{ $self->{USE} }) {
      my @cur_path = $use->_get_inherited_srcdirs ($name);
      unshift @path, @cur_path;
    }
  }

  # Place the path of the current source in the front
  unshift @path, $self->{SRCDIR}{$name}
    if exists $self->{SRCDIR}{$name} and -d $self->{SRCDIR}{$name};

  return @path;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_get_inherited_items ($type);
#
# DESCRIPTION
#   This recursive internal method returns a list containing an inherited
#   build item of the type $type. (Depending of $type, the returned list can
#   be an array or a hash.)
# ------------------------------------------------------------------------------

sub _get_inherited_items {
  my $self = shift;
  my $type = shift;

  return () if not exists $self->{$type};

  if (ref $self->{$type} eq 'ARRAY') {

    my @items = ();

    # Recursively inherit from used builds
    if ($self->{INHERIT}{$type}) {
      for my $use (@{ $self->{USE} }) {
        my @cur_items = $use->_get_inherited_items ($type);

        for my $item (@cur_items) {
          my $type_item = $type . '__' . $item;

          # Check inheritance option of current item
          next unless $self->_inherit_ok ($type_item);

          # The statement ensures that there is no duplication
          push @items, $item unless grep {$_ eq $item} @items;
        }
      }
    }

    # Items in current build
    if (@{ $self->{$type} }) {
      for my $item (@{ $self->{$type} }) {
        # The statement ensures that there is no duplication
        push @items, $item unless grep {$_ eq $item} @items;
      }
    }

    return @items;

  } elsif (ref $self->{$type} eq 'HASH') {

    my %items = ();

    # Recursively inherit from used builds
    if ($self->{INHERIT}{$type}) {
      for my $use (@{ $self->{USE} }) {
        my %cur_items = $use->_get_inherited_items ($type);

        for my $name (keys %cur_items) {
          my $type_name = $type . '__' . $name;

          # Check inheritance option of current item
          next unless $self->_inherit_ok ($type_name);

          # "Closer" ancestors overrides more "distant" ones
          $items{$name} = $cur_items{$name};
        }
      }
    }

    # Items in current build
    if (%{ $self->{$type} }) {
      for my $name (keys %{ $self->{$type} }) {
        # Settings in current build override inherited settings
        $items{$name} = $self->{$type}{$name};
      }
    }

    return %items;

  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_require_pp ($name);
#
# DESCRIPTION
#   This internal method returns true if source package $name requires
#   pre-processing.
# ------------------------------------------------------------------------------

sub _require_pp {
  my $self = shift;
  my $name = $_[0];

  my $rc    = 0;
  my @names = 'PP';
  push @names, (split /__/, $name);

  # Check whether pre-process flag exists, going up the source package hierarchy
  do {
    my $cur_name = join '__', @names;
    if (exists $self->{PP}{$cur_name}) {
      $rc = $self->{PP}{$cur_name};
      return $rc;
    }
  } while pop @names;

  return $rc;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_inherit_ok ($name);
#
# DESCRIPTION
#   This internal method returns true if it is OK to inherit an item specified
#   by $name, (where $name is a double underscore "__" delimited positional
#   list of source package names).
# ------------------------------------------------------------------------------

sub _inherit_ok {
  my $self  = shift;
  my $name  = $_[0];

  my $rc    = 1;
  my @names = split /__/, $name;

  # Check whether INHERIT flag exists, going up the source package hierarchy
  do {
    my $cur_name = join '__', @names;
    if (exists $self->{INHERIT}{$cur_name}) {
      $rc = $self->{INHERIT}{$cur_name};
      return $rc;
    }
  } while pop @names;

  return $rc;
}

# ------------------------------------------------------------------------------

1;

__END__
