#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::SrcFile
#
# DESCRIPTION
#   This class contains methods to manipulate the build process of a source
#   file of supported type.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::SrcFile;

# Standard pragma

use strict;
use warnings;

# Standard modules
use Cwd;
use Carp;
use File::Basename;
use File::Spec;
use File::Spec::Functions;

# FCM component modules
use Fcm::Util;
use Fcm::Timer;

# Other modules
use Ecmwf::Fortran90_stuff ();

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $srcfile = Fcm::SrcFile->new (
#     CONFIG     => $config,
#     SRCPACKAGE => $srcpackage,
#     SRC        => $src,
#     PPSRC      => $ppsrc,
#     TYPE       => $type,
#     SCAN       => $scan,
#     EXEBASE    => $exebase,
#     PCKCFG     => $pckcfg,
#   );
#
# DESCRIPTION
#   This method constructs a new instance of the Fcm::SrcFile class.
#
# ARGUMENTS
#   CONFIG     - reference to a Fcm::Config instance
#   SRCPACKAGE - reference to the container Fcm::SrcPackage instance
#   SRC        - source path of this file
#   PPSRC      - pre-processed source path of this file
#   TYPE       - type flag of this source file
#   SCAN       - scan source file for dependency?
#   EXEBASE    - name of executable
#   PCKCFG     - this source file is modified by a package cfg?
# ------------------------------------------------------------------------------

sub new {
  my $this  = shift;
  my %args  = @_;
  my $class = ref $this || $this;

  my $self = {
    CONFIG     => exists $args{CONFIG}     ? $args{CONFIG}     : &main::cfg,
    SRCPACKAGE => exists $args{SRCPACKAGE} ? $args{SRCPACKAGE} : undef,
    SRC        => exists $args{SRC}        ? $args{SRC}        : undef,
    PPSRC      => exists $args{PPSRC}      ? $args{PPSRC}      : undef,
    TYPE       => exists $args{TYPE}       ? $args{TYPE}       : undef,
    SCAN       => exists $args{SCAN}       ? $args{SCAN}       : 1,
    EXEBASE    => exists $args{EXEBASE}    ? $args{EXEBASE}    : undef,
    PCKCFG     => exists $args{PCKCFG}     ? $args{PCKCFG}     : undef,

    PROGNAME   => undef,
    LANG       => undef,
    DEP        => {}, 
    RULES      => {},
  };
  bless $self, $class;

  return $self;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config = $srcfile->config;
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
#   $srcpackage = $srcfile->srcpackage;
#   $srcfile->srcpackage ($srcpackage);
#
# DESCRIPTION
#   This method returns the reference to the container Fcm::SrcPackage of this
#   source file. If an argument is specified, the reference is set to the
#   value of the argument.
# ------------------------------------------------------------------------------

sub srcpackage {
  my $self = shift;

  if (@_) {
    $self->{SRCPACKAGE} = shift;
  }

  return $self->{SRCPACKAGE};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $src = $srcfile->src;
#   $srcfile->src ($src);
#
# DESCRIPTION
#   This method returns the reference to the location of this source file. If
#   an argument is specified, the location is set to the value of the argument.
# ------------------------------------------------------------------------------

sub src {
  my $self = shift;

  if (@_) {
    $self->{SRC} = shift;
  }

  return $self->{SRC};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ppsrc = $srcfile->ppsrc;
#   $srcfile->ppsrc ($ppsrc);
#
# DESCRIPTION
#   This method returns the reference to the location of the pre-processed
#   file of this source file. If an argument is specified, the location is set
#   to the value of the argument.
# ------------------------------------------------------------------------------

sub ppsrc {
  my $self = shift;

  if (@_) {
    $self->{PPSRC} = shift;
  }

  return $self->{PPSRC};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $time = $srcfile->mtime;
#
# DESCRIPTION
#   This method returns the last modified time of the source file. If a
#   pre-processed version of the source file exists, it returns the last
#   modified time of the pre-processed source file instead.
# ------------------------------------------------------------------------------

sub mtime {
  my $self = shift;

  return $self->{PPSRC} ? (stat $self->{PPSRC})[9] : (stat $self->{SRC})[9];
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $base = $srcfile->base;
#
# DESCRIPTION
#   This method returns the base name of the source file.
# ------------------------------------------------------------------------------

sub base {
  my $self = shift;

  return basename ($self->{SRC});
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ppbase = $srcfile->ppbase;
#
# DESCRIPTION
#   This method returns the base name of the pre-processed source file.
# ------------------------------------------------------------------------------

sub ppbase {
  my $self = shift;

  return basename ($self->{PPSRC});
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $interfacebase = $srcfile->interfacebase;
#
# DESCRIPTION
#   This method returns the base name of the F9X interface file.
# ------------------------------------------------------------------------------

sub interfacebase {
  my $self   = shift;
  my $return = undef;

  if ($self->is_type_or (qw/FORTRAN FPP/) and
      uc ($self->select_tool ('GENINTERFACE')) ne 'NONE' and
      not $self->is_type_or (qw/PROGRAM MODULE/)) {

    my $flag = lc ($self->select_tool ('INTERFACE'));
    my $ext  = $self->config->setting (qw/OUTFILE_EXT INTERFACE/);

    $return = ($flag eq 'program' ? $self->intname : $self->root) . $ext;
  }

  return $return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $root = $srcfile->root;
#
# DESCRIPTION
#   This method returns the root name (i.e. base name without file extension)
#   of the source file.
# ------------------------------------------------------------------------------

sub root {
  my $self = shift;

  (my $root = $self->base) =~ s/\.\w+$//;

  return $root;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ext = $srcfile->ext;
#
# DESCRIPTION
#   This method returns the file extension of the source file.
# ------------------------------------------------------------------------------

sub ext {
  my $self = shift;

  return substr $self->base, length ($self->root);
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ppext = $srcfile->ppext;
#
# DESCRIPTION
#   This method returns the file extension of the pre-processed source file.
# ------------------------------------------------------------------------------

sub ppext {
  my $self = shift;

  return substr $self->ppbase, length ($self->root);
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $dir = $srcfile->dir;
#
# DESCRIPTION
#   This method returns the dir name of the source file.
# ------------------------------------------------------------------------------

sub dir {
  my $self = shift;

  return dirname ($self->{SRC});
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ppdir = $srcfile->ppdir;
#
# DESCRIPTION
#   This method returns the dir name of the pre-processed source file.
# ------------------------------------------------------------------------------

sub ppdir {
  my $self = shift;

  return dirname ($self->{PPSRC});
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $progname = $srcfile->progname();
#   $srcfile->progname ($progname);
#
# DESCRIPTION
#   This method returns the name of the first program unit in a Fortran source
#   file. If an argument is specified, the name is set to the value of the
#   argument.
# ------------------------------------------------------------------------------

sub progname {
  my $self = shift;

  if (@_) {
    $self->{PROGNAME} = $_[0];
  }

  return $self->{PROGNAME};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $intname = $srcfile->intname ();
#
# DESCRIPTION
#   This method returns the internal name of the source file.
# ------------------------------------------------------------------------------

sub intname {
  my $self = shift;

  return $self->progname ? $self->progname : lc ($self->root);
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $objbase = $srcfile->objbase ();
#
# DESCRIPTION
#   If the source file contains a compilable procedure, this method returns
#   the name of the object file.
# ------------------------------------------------------------------------------

sub objbase {
  my $self   = shift;
  my $return = undef;

  if ($self->is_type ('SOURCE')) {
    my $ext = $self->config->setting (qw/OUTFILE_EXT OBJ/);

    if ($self->is_type_or (qw/FORTRAN FPP/)) {
      $return = $self->progname . $ext if $self->progname;

    } else {
      $return = $self->intname . $ext;
    }
  }

  return $return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $donebase = $srcfile->donebase ();
#
# DESCRIPTION
#   This method returns the DONE file for a source file containing a compilable
#   procedure, or the IDONE file for an include file.
# ------------------------------------------------------------------------------

sub donebase {
  my $self   = shift;
  my $return = undef;

  if ($self->is_type ('SOURCE')) {
    if ($self->objbase and not $self->is_type ('PROGRAM')) {
      $return = $self->intname . $self->config->setting (qw/OUTFILE_EXT DONE/);
    }

  } elsif ($self->is_type ('INCLUDE')) {
    $return = $self->base . $self->config->setting (qw/OUTFILE_EXT IDONE/);
  }

  return $return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $exebase = $srcfile->exebase ();
#   $srcfile->exebase ($exebase);
#
# DESCRIPTION
#   If the source file contains a compilable main program, this method returns
#   the executable name. If an argument is specified, the executable name is
#   set to the value of the argument.
# ------------------------------------------------------------------------------

sub exebase {
  my $self = shift;

  if (@_) {
    $self->{EXEBASE} = $_[0];
  }

  my $return;

  if ($self->objbase and $self->is_type ('PROGRAM')) {
    if ($self->config->setting ('EXE_NAME', $self->root)) {
      $return = $self->config->setting ('EXE_NAME', $self->root);

    } elsif ($self->{EXEBASE}) {
      $return = $self->{EXEBASE};

    } else {
      $return = $self->root . $self->config->setting (qw/OUTFILE_EXT EXE/);
    }
  }

  return $return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $base = $srcfile->flagsbase ([$flag]);
#
# DESCRIPTION
#   If the source file contains a compilable program unit, it returns the base
#   name of the compiler flags-file. If $flag is set, it returns the base name
#   of the flags file as specified by $flag. The value of $flag can be:
#     FLAGS   - compiler flags flags-file (default)
#     PPKEYS  - pre-processor keys (i.e. macro definitions) flags-file
#     LD      - linker flags-file
#     LDFLAGS - linker flags flags-file
# ------------------------------------------------------------------------------

sub flagsbase {
  my ($self, $flag) = @_;
  $flag             = 'FLAGS' if not $flag;
  my $return        = undef;

  if ($self->is_type ('SOURCE')) {
    if ($flag eq 'FLAGS' or $flag eq 'PPKEYS') {
      my %src_tool = %{ $self->config->setting ('SRC_TOOL') };

      if ($self->lang and exists $src_tool{$self->lang}{$flag}) {
        $return = join ('__', (
          $src_tool{$self->lang}{$flag}, $self->srcpackage->name, $self->root,
        )) . $self->config->setting (qw/OUTFILE_EXT FLAGS/);
      }

    } elsif ($self->is_type ('PROGRAM')) {
      $return = join ('__', ($flag, $self->srcpackage->name, $self->root)) .
                $self->config->setting (qw/OUTFILE_EXT FLAGS/);
    }
  }

  return $return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   %dep   = $srcfile->dep;
#   @files = $srcfile->dep ($type);
#   $srcfile->dep (\%dep);
#
# DESCRIPTION
#   This method returns the dependencies of this source file. If no argument
#   is set, the method returns the dependency hash of this source file. The
#   keys of the hash are the names of the files this source files depends on
#   and the values of the hash are the dependency types of the corresponding
#   files. If an argument is specified and the argument is a normal string,
#   the method returns the keys of the dependency hash, which have their
#   corresponding values equal to $type. If an argument is specified and the
#   argument is a reference to a hash, the reference to the dependency hash of
#   the current source file is re-set to point to the reference of this new
#   hash.
# ------------------------------------------------------------------------------

sub dep {
  my $self = shift;

  if (@_) {
    if (ref $_[0] eq 'HASH') {
      $self->{DEP} = $_[0];

    } else {
      my $type = $_[0];
      return grep {
        $self->{DEP}{$_} eq $type;
      } keys %{ $self->{DEP} };
    }
  }

  return %{ $self->{DEP} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $srcfile->add_dep ($target, $type);
#
# DESCRIPTION
#   This method adds (or modifies) a dependency to the dependency hash of the
#   source file. The argument $type is the type of the dependency and the
#   argument $target is the dependency target.
# ------------------------------------------------------------------------------

sub add_dep {
  my $self = shift;
  my ($target, $type) = @_;

  $self->{DEP}{$target} = $type;

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @pklist = $self->get_package_list ();
#
# DESCRIPTION
#   This method returns a list of package names associated with this source
#   file. The list begins with the top level container package to the
#   sub-package name of the current source file.
# ------------------------------------------------------------------------------

sub get_package_list {
  my $self = shift;

  my @pknames = ();

  my @packages = split /__/, $self->srcpackage->name;
  push @packages, $self->root;

  for my $i (0 .. $#packages) {
    push @pknames, join ('__', (@packages[0 .. $i]));
  }

  return @pknames;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $pckcfg = $srcfile->pckcfg ();
#   $srcfile->pckcfg ($pckcfg);
#
# DESCRIPTION
#   This method returns the name of the flag to indicate whether this source
#   file is modified by a package level configuration file. If an argument is
#   specified, the flag is set to the value of the argument.
# ------------------------------------------------------------------------------

sub pckcfg {
  my $self = shift;

  if (@_) {
    $self->{PCKCFG} = $_[0];
  }

  return $self->{PCKCFG};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $flag = $srcfile->scan ();
#   $srcfile->scan ($flag);
#
# DESCRIPTION
#   This method returns the "scan" flag that determines whether the source
#   file needs to be scanned for dependency. If an argument is specified, the
#   flag is set to the value of the argument.
# ------------------------------------------------------------------------------

sub scan {
  my $self = shift;

  if (@_) {
    $self->{SCAN} = $_[0];
  }

  return $self->{SCAN};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $type = $srcfile->type;
#   $srcfile->type ($type);
#
# DESCRIPTION
#   This method returns the type flag of the source file. If an argument is
#   specified, the flag is set to the value of the argument.
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
#   $flag = $srcfile->is_type ($type1[, $type2, ...]);
#
# DESCRIPTION
#   This method returns true if current file is a known type matching all the
#   arguments.
# ------------------------------------------------------------------------------

sub is_type {
  my $self    = shift;
  my @intypes = @_;
  my $rc      = 0;

  if ($self->{TYPE}) {
    my @types = split /::/, $self->{TYPE};

    for my $intype (@intypes) {
      $rc = grep {uc $_ eq uc $intype} @types;
      last unless $rc;
    }

  }

  return $rc;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $flag = $srcfile->is_type_or ($type1[, $type2, ...]);
#
# DESCRIPTION
#   This method returns true if current file is a known type matching any of
#   the arguments.
# ------------------------------------------------------------------------------

sub is_type_or {
  my $self    = shift;
  my @intypes = @_;
  my $rc      = 0;

  if ($self->{TYPE}) {
    my @types = split /::/, $self->{TYPE};

    for my $intype (@intypes) {
      $rc = grep {uc $_ eq uc $intype} @types;
      last if $rc;
    }

  }

  return $rc;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $lang = $srcfile->lang ();
#
# DESCRIPTION
#   This method returns the language name of the source file if it contains
#   compilable source of a supported language.
# ------------------------------------------------------------------------------

sub lang {
  my $self = shift;

  if ((not $self->{LANG}) and $self->is_type ('SOURCE')) {
    my %src_tool = %{ $self->config->setting ('SRC_TOOL') };

    for my $key (keys %src_tool) {
      if ($self->is_type ($key)) {
        $self->{LANG} = $key;
        last;
      }
    }
  }

  return $self->{LANG};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $srcfile->determine_type;
#
# DESCRIPTION
#   This method determines whether the source file is a type known to the
#   build system. If so, it sets the "type" flag.
# ------------------------------------------------------------------------------

sub determine_type {
  my $self = shift;

  if (not $self->{TYPE}) {
    # Do not set a type if the file name matches the "ignore" list
    my @ignore = split /,/, $self->config->setting ('INFILE_IGNORE');

    for (@ignore) {
      return if $self->base eq $_;
    }
  }

  if (not $self->{TYPE}) {
    # Determine file type by comparing its extension with supported ones
    my %known_ext = %{ $self->config->setting ('INFILE_EXT') };
    my $ext       = $self->ext ? substr ($self->ext, 1) : 0;
    $self->{TYPE} = $known_ext{$ext} if $ext and exists $known_ext{$ext};
  }

  if (not $self->{TYPE}) {
    # Determine file type by comparing its name with known patterns
    my %known_pat = %{ $self->config->setting ('INFILE_PAT') };
    for my $pat (keys %known_pat) {
      if ($self->base =~ /$pat/) {
        $self->{TYPE} = $known_pat{$pat};
        last;
      }
    }
  }

  if (-s $self->{SRC} and -T $self->{SRC} and not $self->{TYPE}) {
    # Determine file type by inspecting its first line (text file only)
    if (open SRC, '<', $self->{SRC}) {
      my $line = <SRC>;
      close SRC;

      my %known_txt = %{ $self->config->setting ('INFILE_TXT') };
      for my $txt (keys %known_txt) {
        if ($line =~ /^#!.*$txt/) {
          $self->{TYPE} = $known_txt{$txt};
          last;
        }
      }
    }
  }

  if ($self->is_type_or (qw/FORTRAN FPP/)) {
    # Determine whether source file is a main Fortran program or module
    if (open SRC, '<', $self->{SRC}) {
      while (my $line = <SRC>) {
        if ($line =~ /^\s*(PROGRAM|MODULE)\b/i) {
          $self->{TYPE} = $self->{TYPE} . '::' . uc ($1);
          last;

        } elsif ($line =~ /^\s*BLOCK\s*DATA\b/i) {
          $self->{TYPE} = $self->{TYPE} . '::' . 'BLOCKDATA';
          last;
        }
      }
      close SRC;
    }

  } elsif ($self->is_type (qw/C/)) {
    # Determine whether source file is a main C program
    if (open SRC, '<', $self->{SRC}) {
      while (my $line = <SRC>) {
        next unless $line =~ /int\s*main\s*\(/i;
        $self->{TYPE} = $self->{TYPE} . '::PROGRAM';
        last;
      }
      close SRC;
    }
  }

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @pp_src = @{ $srcfile->pre_process () };
#
# DESCRIPTION
#   This method invokes the pre-processor on the source file. It returns a
#   reference to an array containing the lines of the pre-processed source if
#   the pre-processor command succeeded.
# ------------------------------------------------------------------------------

sub pre_process {
  my $self = shift;

  # Support only Fortran and C source files
  return unless $self->is_type_or (qw/FPP C/);

  # List of include directories
  my @inc = @{ $self->config->setting (qw/PATH INC/) };

  # Build the pre-processor command according to file type
  my $name    = $self->is_type ('FPP') ? 'FPP' : 'CPP';
  my %tool    = %{ $self->config->setting ('TOOL') };

  # The pre-processor command and its options
  my @command = ($tool{$name});
  my @ppflags = split /\s+/, $self->select_tool ($name . 'FLAGS');

  # List of defined macros, add "-D" in front of each macro
  my @ppkeys  = split /\s+/, $self->select_tool ($name . 'KEYS');
  @ppkeys     = map {($tool{$name . '_DEFINE' }. $_)} @ppkeys;

  # Add "-I" in front of each include directories
  @inc        = map {($tool{$name . '_INCLUDE'}. $_)} @inc;

  push @command, (@ppflags, @ppkeys, @inc, $self->base);

  my $verbose = $self->config->verbose;
  my $cwd     = cwd;

  # Change to container directory of source file
  print 'cd ', $self->dir, "\n" if $verbose > 1;
  chdir $self->dir;

  # Execute the command, getting the output lines
  my @outlines = &run_command (
    \@command, METHOD => 'qx', PRINT => $verbose > 1, TIME => $verbose > 2,
  );

  # Change back to original directory
  print 'cd ', $cwd, "\n" if $self->config->verbose > 1;
  chdir $cwd;

  return \@outlines;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @interface_block = @{ $srcfile->gen_interface () };
#
# DESCRIPTION
#   This method invokes the Fortran 9x interface block generator to generate
#   an interface block for the current source file. It returns a reference to
#   an array containing the lines of the interface block.
# ------------------------------------------------------------------------------

sub gen_interface {
  my $self = shift;

  my $generator = $self->select_tool ('GENINTERFACE');

  my $src      = $self->{PPSRC} ? $self->{PPSRC} : $self->{SRC};
  my @outlines = ();

  if ($generator eq 'f90aib') {
    # Use F90AIB

    # Open pipeline to interface file generator and read its output
    my $devnull = File::Spec->devnull;
    my $command = $generator;
    $command   .= " <'" . $src . "'" . " 2>'" . $devnull . "'";
    my $croak   = $command . ' failed';

    print timestamp_command ($command, 'Start') if $self->config->verbose > 2;
    open COMMAND, '-|', $command or croak $croak, ' (', $!, '), abort';
    @outlines = readline 'COMMAND';
    close COMMAND or croak $croak, ' (', $?, '), abort';
    print timestamp_command ($command, 'End  ') if $self->config->verbose > 2;

  } elsif ($generator eq 'ECMWF') {
    # Use ECMWF interface generator
  
    # Read source file into an array
    open FILE, '<', $src or croak 'Cannot open "', $src, '" (', $!, '), abort';
    my @src_lines = <FILE>;
    close FILE;
  
    # Process standalone subroutines and functions only
    if (not grep /^\s*(?:program|module)\b/i, @src_lines) {
      print timestamp_command ('Analyse: ' . $self->src, 'Start')
        if $self->config->verbose > 2;

      my @statements = ();
      my %prog_info  = ();
  
      # Set name of source file
      &Ecmwf::Fortran90_stuff::fname ($src);
  
      # Parse lines in source
      &Ecmwf::Fortran90_stuff::setup_parse ();

      # Expand continuation lines in source
      &Ecmwf::Fortran90_stuff::expcont (\@src_lines, \@statements);
  
      # Analyse statements in source
      $Ecmwf::Fortran90_stuff::study_called = 0;
      &Ecmwf::Fortran90_stuff::study (\@statements, \%prog_info);
  
      # Source code is not a module
      if (not $prog_info{is_module}) {
        my @interface_block = ();
        my @line_hash       = ();
  
        # Create an interface block for the program unit
        &Ecmwf::Fortran90_stuff::create_interface_block (
          \@statements,
          \@interface_block,
        );

        # Put continuation lines back
        &Ecmwf::Fortran90_stuff::cont_lines (
          \@interface_block,
          \@outlines,
          \@line_hash,
        );
      }

      print timestamp_command ('Analyse: ' . $self->src, 'End')
        if $self->config->verbose > 2;
    }

  } elsif (uc ($generator) eq 'NONE') {
    print $self->root, ': interface generation is switched off', "\n"
      if $self->config->verbose > 2;

  } else {
    e_report 'Error: Unknown Fortran 9x interface generator: ', $generator, '.';
  }

  return \@outlines;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $tool = $self->select_tool ($name);
#
# DESCRIPTION
#   This method selects the correct "tool" for the current source file by
#   following the name of its container package. The argument $name must be
#   the generic name of the "tool" to be selected. The method returns the
#   value of the selected tool.
# ------------------------------------------------------------------------------

sub select_tool {
  my $self  = shift;
  my $name  = shift;

  return undef unless $name;

  my @pknames = $self->get_package_list ();

  my %tool    = %{ $self->config->setting ('TOOL') };

  for my $pkname (reverse @pknames) {
    my $cur_name = join '__', ($name, $pkname);
    return $tool{$cur_name} if exists $tool{$cur_name};
  }

  return exists $tool{$name} ? $tool{$name} : '';
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = $srcfile->scan_dependency ();
#   $rc = $srcfile->scan_dependency (HEADER_ONLY => 1);
#
# DESCRIPTION
#   This method scans the source file for dependencies. If no argument is
#   specified, the method scans the pre-processed source file if it exists.
#   Otherwise, the original source file is scanned. If HEADER_ONLY is
#   specified, only pre-processing header dependencies are scanned from the
#   source file. (The HEADER_ONLY flag should only be specified if "ppsrc" is
#   not already specified.) This method returns the number of 1 on success.
# ------------------------------------------------------------------------------

sub scan_dependency {
  my $self = shift;
  my %args = @_;

  my $header_only = exists $args{HEADER_ONLY} ? $args{HEADER_ONLY} : 0;

  return 0 unless $self->{SCAN};
  return 0 unless $self->{TYPE};

  my $src = $self->{PPSRC} ? $self->{PPSRC} : $self->{SRC};
  return 0 unless $src;

  # Determine what dependencies are supported by this known type
  my %types = $header_only
              ? %{ $self->config->setting ('PP_DEP_TYPE') }
              : %{ $self->config->setting ('DEP_TYPE') };

  # List of excluded dependencies
  my %excl_dep = %{ $self->config->setting ('EXCL_DEP') };

  # Package list
  my @pknames = $self->get_package_list ();

  my @depends = ();
  for my $key (keys %types) {
    # Check if current file is a type of file requiring dependency scan
    next unless $self->is_type ($key);
    
    # Get list of dependency type for this file
    DEPEND: for my $depend ((split /::/, $types{$key})) {
      # Ignore a dependency type if the dependency is in the exclude list
      if (exists $excl_dep{$depend}) {
        # Global exclude
        next DEPEND if exists $excl_dep{$depend}{''};

        # Sub-package exclude
        for my $pkname (@pknames) {
          next DEPEND if exists $excl_dep{$depend}{$pkname};
        }
      }

      # Add to dependency list for current file
      push @depends, $depend;
    }
  }

  # Scan dependencies, if necessary ...
  if (@depends) {
    # Print diagnostic
    print timestamp_command ('scan dependency in file: ' . $src, 'Start')
      if $self->config->verbose > 2;

    open FILE, '<', $src or croak 'Cannot open "', $src, '" (', $!, ')';
    my @lines = readline 'FILE';
    close FILE;

    # List of dependency patterns
    my %dep_pattern = %{ $self->config->setting ('DEP_PATTERN') };

    LINE: for my $line (@lines) {
      # Ignore empty lines
      next LINE if $line =~ /^\s*$/;

      # Fortran source, also scan for program unit name
      if (! $header_only and ! $self->progname) {
        if ($self->is_type ('SOURCE') and $self->is_type_or (qw/FPP FORTRAN/)) {
          my $pfx_pttn = '(?:(?:ELEMENTAL|(?:RECURSIVE(?:\s+PURE)?|' .
                         'PURE(?:\s+RECURSIVE)?))\s+)?';
          my $spc_pttn = '(?:(?:CHARACTER|COMPLEX|DOUBLE\s*PRECISION|INTEGER|' .
                         'LOGICAL|REAL|TYPE)(?:\s*\(.+\)|\s*\*\d+\s*)??\s+)?';

          if ($line =~ /^\s*PROGRAM\s+(\w+)/i) {
            # Matches the beginning of a named main program
            $self->progname (lc $1);
            next LINE;

          } elsif ($line =~ /^\s*MODULE\s+(\w+)/i) {
            my $keyword = $1;

            if (uc ($keyword) ne 'PROCEDURE') {
              # Matches the beginning of a module
              $self->progname (lc $keyword);
              next LINE;
            }

          } elsif ($line =~ /^\s*BLOCK\s*DATA\s+(\w+)/i) {
            # Matches the beginning of a named block data program unit
            $self->progname (lc $1);
            next LINE;

          } elsif ($line =~ /^\s*$pfx_pttn SUBROUTINE\s+(\w+)/ix) {
            # Matches the beginning of a subroutine
            $self->progname (lc $1);
            next LINE;

          } elsif ($line =~ /^\s*$pfx_pttn $spc_pttn FUNCTION\s+(\w+)/ix) {
            # Matches the beginning of a function
            $self->progname (lc $1);
            next LINE;
          }
        }
      }

      # Scan known dependencies
      for my $depend (@depends) {
        # Check if a pattern exists for the current dependency
        next unless exists $dep_pattern{$depend};

        # Attempt to match the pattern
        my $pattern = $dep_pattern{$depend};

        if ($line =~ /$pattern/i) {
          my $match = $1;

          # $match may contain multiple items delimited by space
          NAME: for my $name (split /\s+/, $match) {
            # Skip dependency if it is in the exclusion list
            my $key = uc ($depend . '::' . $name);

            if (exists $excl_dep{$key}) {
              # Exclude this dependency, in the global list
              next NAME if exists $excl_dep{$key}{''};

              # Exclude this dependency, current sub-package
              for my $pkname (@pknames) {
                next NAME if exists $excl_dep{$key}{$pkname};
              }
            }

            # Add this dependency to the list
            $self->add_dep ($name, $depend);
          }

          next LINE;
        }
      }
    }

    # Diagnostic messages
    if ($self->config->verbose > 2) {
      my $base = $self->ppsrc ? $self->ppbase : $self->base;

      print $self->srcpackage->name, ': ', $base;
      print ': scanned ', scalar (@lines), ' lines for ';
      print 'header ' if $header_only;
      print 'dependencies: ', scalar (keys %{ $self->{DEP} }), "\n";
      print timestamp_command ('scan dependency in file: ' . $src, 'End');
    }
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   %rules = $srcfile->required_rules ();
#
# DESCRIPTION
#   This method returns a hash in the following format:
#     %rules = (
#       target => {ACTION => action, DEP => [dependencies], ...},
#       ...    => {...},
#     );
#   where the 1st rank keys are the available targets for building this source
#   file, the second rank keys are ACTION and DEP. The value of ACTION is the
#   action for building the target, which can be "COMPILE", "LOAD", "TOUCH",
#   "CP" or "AR". The value of DEP is a refernce to an array containing a list
#   of dependencies suitable for insertion into the Makefile.
# ------------------------------------------------------------------------------

sub required_rules {
  my $self = shift;

  if (not keys %{ $self->{RULES} }) {
    my %outfile_ext = %{ $self->config->setting ('OUTFILE_EXT') };

    if ($self->is_type (qw/SOURCE/)) {
      # Source file
      # ------------------------------------------------------------------------
      # Determine the whether the language of the source file is supported
      my %src_tool = %{ $self->config->setting ('SRC_TOOL') };

      return () unless $self->lang;

      # Compile object
      # ------------------------------------------------------------------------
      if ($self->objbase) {
        # Depends on the source file
        my @dep = ($self->_makerule_srcfile);

        # Depends on the compiler flags flags-file
        my @flags;
        push @flags, ('FLAGS' )
          if $self->flagsbase ('FLAGS' );
        push @flags, ('PPKEYS')
          if $self->flagsbase ('PPKEYS') and not $self->ppsrc;

        push @dep, $self->flagsbase ($_) for (@flags);

        # Source file dependencies
        for my $name (sort keys %{ $self->{DEP} }) {
          # A Fortran 9X module, lower case object file name
          if ($self->{DEP}{$name} eq 'USE') {
            (my $root = $name) =~ s/\.\w+$//;
            push @dep, lc ($root) . $outfile_ext{OBJ};

          # An include file
          } elsif ($self->{DEP}{$name} =~ /^(?:INC|H|INTERFACE)$/) {
            push @dep, $name;
          }
        }

        $self->{RULES}{$self->objbase} = {ACTION => 'COMPILE', DEP => \@dep};

        # Touch flags-files
        # ----------------------------------------------------------------------
        for my $flag (@flags) {
          next unless $self->flagsbase ($flag);

          $self->{RULES}{$self->flagsbase ($flag)} = {
            ACTION => 'TOUCH',
            DEP    => [
              $self->srcpackage->flagsbase ($src_tool{$self->lang}{$flag}),
            ],
            DEST   => '$(FCM_FLAGSDIR)',
          };
        }
      }

      if ($self->exebase) {
        # Link into an executable
        # ----------------------------------------------------------------------
        my @dep = ();
        push @dep, $self->objbase               if $self->objbase;
        push @dep, $self->flagsbase ('LD'     ) if $self->flagsbase ('LD'     );
        push @dep, $self->flagsbase ('LDFLAGS') if $self->flagsbase ('LDFLAGS');

        # Depends on BLOCKDATA program units, for Fortran programs
        my %blockdata = %{ $self->config->setting ('BLOCKDATA') };
        my @blkobj    = ();

        if ($self->is_type_or (qw/FPP FORTRAN/) and keys %blockdata) {
          # List of BLOCKDATA object files
          if (exists $blockdata{$self->exebase}) {
            @blkobj = keys (%{ $blockdata{$self->exebase} });

          } elsif (exists $blockdata{''}) {
            @blkobj = keys (%{ $blockdata{''} });
          }

          for my $name (@blkobj) {
            (my $root = $name) =~ s/\.\w+$//;
            $name = $root . $outfile_ext{OBJ};
            push @dep, $root . $outfile_ext{DONE};
          }
        }

        # Extra executable dependencies
        my %exe_dep = %{ $self->config->setting ('EXE_DEP') };
        if (keys %exe_dep) {
          my @exe_deps;
          if (exists $exe_dep{$self->exebase}) {
            @exe_deps = keys (%{ $exe_dep{$self->exebase} });

          } elsif (exists $exe_dep{''}) {
            @exe_deps = keys (%{ $exe_dep{''} });
          }

          my $pattern = '\\' . $outfile_ext{OBJ} . '$';

          for my $name (@exe_deps) {
            if ($name =~ /$pattern/) {
              # Extra dependency is an object
              (my $root = $name) =~ s/\.\w+$//;
              push @dep, $root . $outfile_ext{DONE};

            } else {
              # Extra dependency is a sub-package
              my $var;
              if ($self->config->setting ('FCM_PCK_OBJECTS', $name)) {
                # sub-package name contains unusual characters
                $var = $self->config->setting ('FCM_PCK_OBJECTS', $name);

              } else {
                # sub-package name contains normal characters
                $var = $name ? join ('__', ('OBJECTS', $name)) : 'OBJECTS';
              }

              push @dep, '$(' . $var . ')';
            }
          }
        }

        # Source file dependencies
        for my $name (sort keys %{ $self->{DEP} }) {
          (my $root = $name) =~ s/\.\w+$//;

          # Lowercase name for object dependency
          $root = lc ($root) unless $self->{DEP}{$name} =~ /^(?:INC|H)$/;

          # Select "done" file extension
          if ($self->{DEP}{$name} =~ /^(?:INC|H)$/) {
            push @dep, $name . $outfile_ext{IDONE};

          } else {
            push @dep, $root . $outfile_ext{DONE};
          }
        }

        $self->{RULES}{$self->exebase} = {
          ACTION => 'LOAD', DEP => \@dep, BLOCKDATA => \@blkobj,
        };

        # Touch Linker flags-file
        # ----------------------------------------------------------------------
        for my $flag (qw/LD LDFLAGS/) {
          $self->{RULES}{$self->flagsbase ($flag)} = {
            ACTION => 'TOUCH',
            DEP    => [$self->srcpackage->flagsbase ($flag)],
            DEST   => '$(FCM_FLAGSDIR)',
          };
        }

      }

      if ($self->donebase) {
        # Touch done file
        # ----------------------------------------------------------------------
        my @dep = ($self->objbase);

        for my $name (sort keys %{ $self->{DEP} }) {
          (my $root = $name) =~ s/\.\w+$//;

          # Lowercase name for object dependency
          $root = lc ($root) unless $self->{DEP}{$name} =~ /^(?:INC|H)$/;

          # Select "done" file extension
          if ($self->{DEP}{$name} =~ /^(?:INC|H)$/) {
            push @dep, $name . $outfile_ext{IDONE};

          } else {
            push @dep, $root . $outfile_ext{DONE};
          }
        }

        $self->{RULES}{$self->donebase} = {
          ACTION => 'TOUCH', DEP => \@dep, DEST => '$(FCM_DONEDIR)',
        };
      }
      
      if ($self->interfacebase) {
        # Interface target
        # ----------------------------------------------------------------------
        # Source file dependencies
        my @dep = ();
        for my $name (sort keys %{ $self->{DEP} }) {
          # Depends on Fortran 9X modules
          push @dep, lc ($name) . $outfile_ext{OBJ}
            if $self->{DEP}{$name} eq 'USE';
        }

        $self->{RULES}{$self->interfacebase} = {DEP => \@dep};
      }

    } elsif ($self->is_type ('INCLUDE')) {
      # Copy include target
      # ------------------------------------------------------------------------
      my @dep = ($self->_makerule_srcfile);

      for my $name (sort keys %{ $self->{DEP} }) {
        # A Fortran 9X module, lower case object file name
        if ($self->{DEP}{$name} eq 'USE') {
          (my $root = $name) =~ s/\.\w+$//;
          push @dep, lc ($root) . $outfile_ext{OBJ};

        # An include file
        } elsif ($self->{DEP}{$name} =~ /^(?:INC|H|INTERFACE)$/) {
          push @dep, $name;
        }
      }

      $self->{RULES}{$self->base} = {
        ACTION => 'CP', DEP => \@dep, DEST => '$(FCM_INCDIR)',
      };

      # Touch IDONE file
      # ------------------------------------------------------------------------
      if ($self->donebase) {
        my @dep = ($self->_makerule_srcfile);

        for my $name (sort keys %{ $self->{DEP} }) {
          (my $root = $name) =~ s/\.\w+$//;

          # Lowercase name for object dependency
          $root   = lc ($root) unless $self->{DEP}{$name} =~ /^(?:INC|H)$/;

          # Select "done" file extension
          if ($self->{DEP}{$name} =~ /^(?:INC|H)$/) {
            push @dep, $name . $outfile_ext{IDONE};

          } else {
            push @dep, $root . $outfile_ext{DONE};
          }
        }

        $self->{RULES}{$self->donebase} = {
          ACTION => 'TOUCH', DEP => \@dep, DEST => '$(FCM_DONEDIR)',
        };
      }

    } elsif ($self->is_type_or (qw/EXE SCRIPT/)) {
      # Copy executable file
      # ------------------------------------------------------------------------
      my @dep = ($self->_makerule_srcfile);

      # Depends on dummy copy file, if file is an "always build type"
      push @dep, $self->config->setting (qw/MISC CPDUMMY/)
        if $self->is_type_or (
          split (/,/, $self->config->setting ('ALWAYS_BUILD_TYPE'))
        );

      # Depends on other executable files
      for my $name (sort keys %{ $self->{DEP} }) {
        push @dep, $name if $self->{DEP}{$name} eq 'EXE';
      }

      $self->{RULES}{$self->base} = {
        ACTION => 'CP', DEP => \@dep, DEST => '$(FCM_BINDIR)',
      };

    } elsif ($self->is_type ('LIB')) {
      # Archive object library
      # ------------------------------------------------------------------------
      my @dep;
      for my $name (sort keys %{ $self->{DEP} }) {
        next unless $self->{DEP}{$name} eq 'OBJ';

        if ($name =~ /^\$\(\w+\)$/) {
          # Dependency is a Makefile variable
          push @dep, $name;

        } else {
          # Dependency is an object
          (my $root = $name) =~ s/\.\w+$//;
          push @dep, lc ($root) . $outfile_ext{OBJ};
        }
      }

      $self->{RULES}{$self->base} = {ACTION => 'AR', DEP => \@dep};
    }
  }

  return %{ $self->{RULES} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $string = $srcfile->write_makerules ();
#
# DESCRIPTION
#   This method returns a string containing the "Make" rules for building the
#   source file.
# ------------------------------------------------------------------------------

sub write_makerules {
  my $self  = shift;
  my $mk    = '';
  my %rules = $self->required_rules;
  my $nl    = " \\\n" . ' ' x 10;

  for my $target (sort keys %rules) {
    $mk .= $target . ':';
    
    for my $dep (@{ $rules{$target}{DEP} }) {
      $mk .= $nl . $dep;
    }

    $mk .= "\n";

    if (exists $rules{$target}{ACTION}) {
      if ($rules{$target}{ACTION} eq 'COMPILE') {
        if ($self->lang) {
          $mk .= "\t" . 'fcm_internal compile:' . substr ($self->lang, 0, 1) .
                 ' ' . $self->srcpackage->name . ' $< $@';
          $mk .= ' 1' if ($self->flagsbase ('PPKEYS') and not $self->ppsrc);
          $mk .= "\n";
        }

      } elsif ($rules{$target}{ACTION} eq 'LOAD') {
        $mk .= "\t" . 'fcm_internal load ' . $self->srcpackage->name . ' $< $@';
        $mk .= ' ' . join (' ', @{ $rules{$target}{BLOCKDATA} })
          if @{ $rules{$target}{BLOCKDATA} };
        $mk .= "\n";

      } elsif ($rules{$target}{ACTION} eq 'TOUCH') {
        $mk .= "\t" . 'touch ' . catfile ($rules{$target}{DEST}, '$@') . "\n";

      } elsif ($rules{$target}{ACTION} eq 'CP') {
        $mk .= "\t" . 'cp $< ' . $rules{$target}{DEST} . "\n";
        $mk .= "\t" . 'chmod u+w ' . catfile ($rules{$target}{DEST}, '$@') . "\n";

      } elsif ($rules{$target}{ACTION} eq 'AR') {
        $mk .= "\t" . 'fcm_internal archive $@ $(^F)' . "\n";
      }
    }

    $mk .= "\n";
  }

  return $mk;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $string = $srcfile->_makerule_srcfile ();
#
# DESCRIPTION
#   This internal method returns a string containing the location of the
#   source file relative to a package source path. This string will be
#   suitable for use in a "Make" rule file for FCM.
# ------------------------------------------------------------------------------

sub _makerule_srcfile {
  my $self = shift;

  my $return;
  my @searchpath;
  my $label;
  my $dir;
  my $base;

  if ($self->ppsrc) {
    $return     = $self->ppsrc;
    @searchpath = $self->srcpackage->ppsearchpath;
    $label      = 'PPSRCDIR';
    $dir        = $self->ppdir;
    $base       = $self->ppbase;

  } else {
    $return     = $self->src;
    @searchpath = $self->srcpackage->searchpath;
    $label      = 'SRCDIR';
    $dir        = $self->dir;
    $base       = $self->base;
  }

  $return = catfile $dir, $base;

  # Use variable for directory name
  # if container package name contains word characters only
  if ($self->srcpackage->name =~ /^\w+$/) {
    for my $i (0 .. $#searchpath) {
      if ($dir eq $searchpath[$i]) {
        my $returndir = '$(' . $label . $i . '__' . $self->srcpackage->name .
                        ')';
        $return = catfile $returndir, $base;
        last;
      }
    }
  }

  return $return;
}

# ------------------------------------------------------------------------------

1;

__END__
