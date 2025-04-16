#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::BuildTask
#
# DESCRIPTION
#   This class hosts information of a build task in the FCM build system.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::BuildTask;

# Standard pragma
use strict;
use warnings;

# Standard modules
use Carp;
use File::Compare;
use File::Basename;
use File::Path;
use File::Spec::Functions;

# FCM component modules
use Fcm::Util;
use Fcm::Timer;

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $task = Fcm::BuildTask->new (
#     CONFIG     => $config,
#     TARGET     => $target,
#     TARGETPATH => \@targetpath,
#     SRCFILE    => $srcfile,
#     DEPENDENCY => \@dependency,
#     TASKTYPE   => $tasktype,
#   );
#
# DESCRIPTION
#   This method constructs a new instance of the Fcm::BuildTask class.
#
# ARGUMENTS
#   CONFIG     - reference to a Fcm::Config instance
#   TARGET     - target name for this task
#   TARGETPATH - search path for the target
#   SRCFILE    - reference to input Fcm::SrcFile instance
#   DEPENDENCY - list of dependencies for this target
#   TASKTYPE   - type of task to build the target
# ------------------------------------------------------------------------------

sub new {
  my $this  = shift;
  my %args  = @_;
  my $class = ref $this || $this;

  my $self  = {
    CONFIG      => exists $args{CONFIG}     ? $args{CONFIG}     : &main::cfg,
    TARGET      => exists $args{TARGET}     ? $args{TARGET}     : undef,
    TARGETPATH  => exists $args{TARGETPATH} ? $args{TARGETPATH} : [],
    SRCFILE     => exists $args{SRCFILE}    ? $args{SRCFILE}    : undef,
    DEPENDENCY  => exists $args{DEPENDENCY} ? $args{DEPENDENCY} : [],
    ACTIONTYPE  => exists $args{ACTIONTYPE} ? $args{ACTIONTYPE} : undef,

    OUTPUT      => undef,
    OUTPUTMTIME => undef,
  };

  bless $self, $class;
  return $self;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config = $task->config;
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
#   $srcfile = $task->srcfile;
#   $task->srcfile ($srcfile);
#
# DESCRIPTION
#   This method returns the reference to the input Fcm::SrcFile instance
#   associated with this task. If an argument is specified, the reference is
#   modified to the value given by the argument.
# ------------------------------------------------------------------------------

sub srcfile {
  my $self = shift;

  if (@_) {
    $self->{SRCFILE} = $_[0];
  }

  return $self->{SRCFILE};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $actiontype = $task->actiontype;
#   $task->actiontype ($actiontype);
#
# DESCRIPTION
#   This method returns the action type of this task. If an argument is
#   specified, the action type of this task is set to the value of the
#   argument.
# ------------------------------------------------------------------------------

sub actiontype {
  my $self = shift;

  if (@_) {
    $self->{ACTIONTYPE} = $_[0];
  }

  return $self->{ACTIONTYPE};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $output = $task->output;
#   $task->output ($output);
#
# DESCRIPTION
#   This method returns the name of the output file after the task has been
#   performed successfully. If an argument is specified, it sets the output
#   file (and its last modified time) to the value of the argument.
# ------------------------------------------------------------------------------

sub output {
  my $self = shift;

  if (@_) {
    $self->{OUTPUT}      = $_[0];
    $self->{OUTPUTMTIME} = (stat $self->{OUTPUT}) [9];
  }

  return $self->{OUTPUT};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $time = $task->outputmtime;
#
# DESCRIPTION
#   This method returns the modified time of the output file.
# ------------------------------------------------------------------------------

sub outputmtime {
  my $self = shift;

  return $self->{OUTPUTMTIME};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = $task->action (TASKLIST => \%tasklist);
#
# DESCRIPTION
#   This method performs the task action and sets the output accordingly. The
#   argument TASKLIST must be a reference to a hash containing the other tasks
#   of the build, which this task may depend on. The keys of the hash must the
#   name of the target names of the tasks, and the values of the hash must be
#   the references to the corresponding Fcm::BuildTask instances. The method
#   returns true if the task has been performed to create a new version of the
#   target.
# ------------------------------------------------------------------------------

sub action {
  my $self     = shift;
  my %args     = @_;
  my $tasklist = exists $args{TASKLIST} ? $args{TASKLIST} : {};

  return unless $self->actiontype;

  my $uptodate     = 1;
  my $dep_uptodate = 1;

  # Check if dependencies are up to date
  # ----------------------------------------------------------------------------
  for my $depend (@{ $self->{DEPENDENCY} }) {
    if (exists $tasklist->{$depend}) {
      if (not $tasklist->{$depend}->output) {
        # Dependency task output is not set, performs its task action
        if ($tasklist->{$depend}->action (TASKLIST => $tasklist)) {
          $uptodate     = 0;
          $dep_uptodate = 0;
        }
      }

    } elsif ($self->config->verbose > 1) {
      w_report 'Warning: Task for "', $depend,
               '" does not exist, may be required by ', $self->{TARGET};
    }
  }

  # Check if the target exists in the search path
  # ----------------------------------------------------------------------------
  if (@{ $self->{TARGETPATH} }) {
    my $output = find_file_in_path ($self->{TARGET}, $self->{TARGETPATH});
    $self->output ($output) if $output;
  }

  # Target is out of date if it does not exist
  if ($uptodate) {
    $uptodate = 0 if not $self->output;
  }

  # Check if current target is older than its dependencies
  # ----------------------------------------------------------------------------
  if ($uptodate) {
    for my $depend (@{ $self->{DEPENDENCY} }) {
      next unless exists $tasklist->{$depend};

      if ($tasklist->{$depend}->outputmtime > $self->outputmtime) {
        $uptodate     = 0;
        $dep_uptodate = 0;
      }
    }

    if ($uptodate and ref $self->srcfile) {
      $uptodate = 0 if $self->srcfile->mtime > $self->outputmtime;
    }
  }

  if ($uptodate) {
    # Current target and its dependencies are up to date
    # --------------------------------------------------------------------------
    if ($self->actiontype eq 'PP') {
      # "done" file up to date, set name of pre-processed source file
      # ------------------------------------------------------------------------
      my $base     = $self->srcfile->root . lc ($self->srcfile->ext);
      my @pck_list = $self->srcfile->get_package_list;
      pop @pck_list;
      my @pknames  = split '__', pop (@pck_list);
      my @path     = map {
        catfile ($_, @pknames);
      } @{ $self->config->setting (qw/PATH PPSRC/) };
      my $oldfile = find_file_in_path ($base, \@path);
      $self->srcfile->ppsrc ($oldfile);
    }

  } else {
    # Perform action is not up to date
    # --------------------------------------------------------------------------
    # (For GENINTERFACE and PP, perform action if "done" file not up to date)
    my $new_output = @{ $self->{TARGETPATH} }
                     ? catfile ($self->{TARGETPATH}[0], $self->{TARGET})
                     : $self->{TARGET};

    # Create destination container directory if necessary
    my $destdir = dirname $new_output;

    if (not -d $destdir) {
      print 'Make directory: ', $destdir, "\n" if $self->config->verbose > 2;
      mkpath $destdir;
    }

    # List of actions
    if ($self->actiontype eq 'UPDATE') {
      # Action is UPDATE: Update file
      # ------------------------------------------------------------------------
      print 'Update: ', $new_output, "\n" if $self->config->verbose > 2;
      touch_file $new_output
        or croak 'Unable to update "', $new_output, '", abort';
      $self->output ($new_output);

    } elsif ($self->actiontype eq 'COPY') {
      # Action is COPY: copy file to destination if necessary
      # ------------------------------------------------------------------------
      my $copy_required = ($dep_uptodate and $self->output and -r $self->output)
                          ? compare ($self->output, $self->srcfile->src)
                          : 1;

      if ($copy_required) {
        # Set up copy command
        &run_command (
          ['cp', $self->srcfile->src, $destdir],
          TIME => $self->config->verbose > 2,
        );

        $self->output ($new_output);

      } else {
        $uptodate = 1;
      }

    } elsif ($self->actiontype eq 'PP' or $self->actiontype eq 'GENINTERFACE') {
      # Action is PP or GENINTERFACE: process file
      # ------------------------------------------------------------------------
      my ($newlines, $base, @path);

      if ($self->actiontype eq 'PP') {
        # Invoke the pre-processor on the source file
        # ----------------------------------------------------------------------
        # Get lines in the pre-processed source
        $newlines = $self->srcfile->pre_process;
        $base     = $self->srcfile->root . lc ($self->srcfile->ext);

        # Get search path for the existing pre-processed file
        my @pck_list = $self->srcfile->get_package_list;
        pop @pck_list;
        my @pknames  = split '__', pop (@pck_list);
        @path        = map {
          catfile ($_, @pknames);
        } @{ $self->config->setting (qw/PATH PPSRC/) };

      } else { # if ($self->actiontype eq 'GENINTERFACE')
        # Invoke the interface generator
        # ----------------------------------------------------------------------
        # Get new interface lines
        $newlines = $self->srcfile->gen_interface;

        # Get search path for the existing interface file
        $base     = $self->srcfile->interfacebase;
        @path     = @{ $self->config->setting (qw/PATH INC/) },
      }


      # If pre-processed or interface file exists,
      # compare its content with new lines to see if it has been updated
      my $update_required = 1;
      my $oldfile = find_file_in_path ($base, \@path);

      if ($oldfile and -r $oldfile) {
        # Read old file
        open FILE, '<', $oldfile;
        my @oldlines = readline 'FILE';
        close FILE;

        # Compare old contents and new contents
        if (@oldlines eq @$newlines) {
          $update_required = grep {
            $oldlines[$_] ne $newlines->[$_];
          } (0 .. $#oldlines);
        }
      }

      if ($update_required) {
        # Update the pre-processed source or interface file
        # ----------------------------------------------------------------------
        # Determine container directory of the  pre-processed or interface file
        my $newfile = @path ? catfile ($path[0], $base) : $base;

        # Create the container directory if necessary
        if (not -d $path[0]) {
          print 'Make directory: ', $path[0], "\n"
            if $self->config->verbose > 1;
          mkpath $path[0];
        }

        # Update the pre-processor or interface file
        open FILE, '>', $newfile
          or croak 'Cannot write to "', $newfile, '" (', $!, '), abort';
        print FILE @$newlines;
        close FILE
          or croak 'Cannot write to "', $newfile, '" (', $!, '), abort';
        print 'Generated: ', $newfile, "\n" if $self->config->verbose > 1;

        # Set the name of the pre-processed file
        $self->srcfile->ppsrc ($newfile) if $self->actiontype eq 'PP';

        # Set the "current" flag of the container source package to "true"
        $self->srcfile->srcpackage->current (1);

      } else {
        # Content in pre-processed source or interface file is up to date
        # ----------------------------------------------------------------------
        $uptodate = 1;

        # Set the name of the pre-processed file
        $self->srcfile->ppsrc ($oldfile) if $self->actiontype eq 'PP';
      }

      # Update the "done" file
      print 'Update: ', $new_output, "\n" if $self->config->verbose > 2;
      touch_file $new_output
        or croak 'Unable to update "', $new_output, '", abort';
      $self->output ($new_output);

    } else {
      carp 'Action type "', $self->actiontype, "' not supported";
    }
  }

  return not $uptodate;
}

# ------------------------------------------------------------------------------

1;

__END__
