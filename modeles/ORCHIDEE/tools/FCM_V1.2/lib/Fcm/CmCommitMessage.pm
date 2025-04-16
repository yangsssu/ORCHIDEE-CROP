#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::CmCommitMessage
#
# DESCRIPTION
#   This class contains methods to read, write and edit the commit message file
#   in a working copy.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::CmCommitMessage;

# Standard pragma
use warnings;
use strict;

# Standard modules
use Carp;
use Cwd;
use File::Spec;
use File::Temp qw/tempfile/;

# FCM component modules
use Fcm::Util qw/e_report run_command/;

# Commit log delimiter messages
my $ignore_line = '--This line, and those below, will be ignored--';
my $auto_line   = '--This line will be ignored and those below will be ' .
                  'inserted automatically--';

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ci_mesg = Fcm::CmCommitMessage->new (
#     CONFIG  => $config,
#     DIR     => $dir,
#     BASE    => $base,
#   );
#
# DESCRIPTION
#   This method constructs a new instance of the Fcm::CmCommitMessageFile class.
#
# ARGUMENTS
#   CONFIG - reference to a Fcm::Config instance
#   DIR    - directory containing the commit message file
#   BASE   - base name of the commit message file
# ------------------------------------------------------------------------------

sub new {
  my $this  = shift;
  my %args  = @_;
  my $class = ref $this || $this;

  my $self = {
    CONFIG => (exists $args{CONFIG} ? $args{CONFIG} : &main::cfg),
    DIR    => (exists $args{DIR}    ? $args{DIR}    : cwd),
    BASE   => (exists $args{BASE}   ? $args{BASE}   : '#commit_message#'),
    USER_M => [],
    AUTO_M => [],
    IGNORE => [],
  };

  bless $self, $class;
  return $self;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config = $ci_mesg->config;
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
#   $dir = $ci_mesg->dir;
#   $ci_mesg->dir ($dir);
#
# DESCRIPTION
#   This method returns the directory container of the commit message file. If
#   an argument is specified, the directory is reset using the value of the
#   argument. (It does nothing is the directory does not already exist.)
# ------------------------------------------------------------------------------

sub dir {
  my $self = shift;

  if (@_) {
    my $dir = shift;
    $self->{DIR} = $dir if -d $dir;
  }

  return $self->{DIR};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $base = $ci_mesg->base;
#   $ci_mesg->base ($base);
#
# DESCRIPTION
#   This method returns the base name of the commit message file. If an
#   argument is specified, the file is reset using the value of the argument.
# ------------------------------------------------------------------------------

sub base {
  my $self = shift;

  if (@_) {
    $self->{BASE} = shift;
  }

  return $self->{BASE};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $file = $ci_mesg->file;
#   $ci_mesg->file ($file);
#
# DESCRIPTION
#   This method returns the full name of the commit message file. If an
#   argument is specified, the file is reset using the value of the argument.
# ------------------------------------------------------------------------------

sub file {
  my $self = shift;

  if (@_) {
    my $file      = shift;
    $self->{DIR}  = dirname  ($file);
    $self->{BASE} = basename ($file);
  }

  return File::Spec->catfile ($self->{DIR}, $self->{BASE});
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @mesg = $ci_mesg->user_mesg;
#   $ci_mesg->user_mesg (@mesg);
#
# DESCRIPTION
#   This method returns the user defined commit message. If an argument is
#   specified, the message is reset using the value of the argument.
# ------------------------------------------------------------------------------

sub user_mesg {
  my $self = shift;

  if (@_) {
    @{ $self->{USER_M} } = @_;
  }

  return @{ $self->{USER_M} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @mesg = $ci_mesg->auto_mesg;
#   $ci_mesg->auto_mesg (@mesg);
#
# DESCRIPTION
#   This method returns the automatically inserted commit message. If an
#   argument is specified, the message is reset using the value of the
#   argument.
# ------------------------------------------------------------------------------

sub auto_mesg {
  my $self = shift;

  if (@_) {
    @{ $self->{AUTO_M} } = @_;
  }

  return @{ $self->{AUTO_M} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @mesg = $ci_mesg->ignore_mesg;
#   $ci_mesg->ignore_mesg (@mesg);
#
# DESCRIPTION
#   This method returns the ignored part of a commit message. If an argument is
#   specified, the message is reset using the value of the argument.
# ------------------------------------------------------------------------------

sub ignore_mesg {
  my $self = shift;

  if (@_) {
    @{ $self->{IGNORE} } = @_;
  }

  return @{ $self->{IGNORE} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   ($user, $auto) = $ci_mesg->read_file ();
#
# DESCRIPTION
#   This function reads from the commit log message file. It resets the user
#   and the automatic messages after reading the file. It returns the message
#   back in two array references.
# ------------------------------------------------------------------------------

sub read_file {
  my $self = shift;

  my @user = ();
  my @auto = ();
  my $file = $self->file;

  if (-r $file) {
    open FILE, '<', $file or croak 'Cannot open ', $file, '(', $!, '), abort';

    my $in_auto = 0;
    while (<FILE>) {
      last if index ($_, $ignore_line) == 0; # ignore after the ignore delimiter

      if (index ($_, $auto_line) == 0) {
        # Beginning of the automatically inserted message delimiter
        $in_auto = 1;
        next;
      }

      if ($in_auto) {
        push @auto, $_;

      } else {
        push @user, $_;
      }
    }

    close FILE;

    $self->user_mesg (@user);
    $self->auto_mesg (@auto);
  }

  return (\@user, \@auto);
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ci_mesg->write_file ();
#
# DESCRIPTION
#   This function writes to the commit log message file based on the content of
#   the user defined message, and the automatically inserted message.
# ------------------------------------------------------------------------------

sub write_file {
  my $self = shift;
  my %args = @_;

  my @user = $self->user_mesg;
  my @auto = $self->auto_mesg;
  my $file = $self->file;

  open FILE, '>', $file or die 'Cannot open ', $file, '(', $!, '), abort';
  print FILE @user;
  print FILE $auto_line, "\n", @auto if @auto;
  close FILE or croak 'Cannot close ', $file, '(', $!, '), abort';

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $file = $ci_mesg->edit_file ([TEMP => 1,] [BATCH => 1,]);
#
# DESCRIPTION
#   This function normally triggers an editor for editing the commit message.
#   If TEMP is set, it edits a temporary file. Otherwise, it edits the current
#   commit message file. It resets the user defined message on success. Returns
#   the name of the commit log file. Do not start the editor if BATCH is set.
# ------------------------------------------------------------------------------

sub edit_file {
  my $self  = shift;
  my %args  = @_;
  my $temp  = exists $args{TEMP}  ? $args{TEMP}  : 0;
  my $batch = exists $args{BATCH} ? $args{BATCH} : 0;

  my @user   = $self->user_mesg;
  my @auto   = $self->auto_mesg;
  my @ignore = $self->ignore_mesg;
  my $file   = $self->file;

  if ($temp) {
    my $fh;
    ($fh, $file) = tempfile (SUFFIX => ".fcm", UNLINK => 1);
    close $fh;
  }

  # Add original or code driven message and status information to the file
  my $select = select;
  open FILE, '>', $file or croak 'Cannot open ', $file, ' (', $!, '), abort';
  select FILE;

  print @user;
  print (@auto || @user ? '' : "\n");
  print $auto_line, "\n", @auto, "\n" if @auto;
  print $ignore_line, "\n\n";
  print @ignore if @ignore;

  close FILE or die 'Cannot close ', $file, ' (', $!, '), abort';
  select $select;

  if (not $batch) {
    # Select editor
    my $editor = 'nedit';
    
    if ($ENV{'SVN_EDITOR'}) {
      $editor = $ENV{'SVN_EDITOR'};

    } elsif ($ENV{'VISUAL'}) {
      $editor = $ENV{'VISUAL'};

    } elsif ($ENV{'EDITOR'}) {
      $editor = $ENV{'EDITOR'};
    }

    # Execute command to start the editor
    print 'Starting ', $editor, ' to edit commit message ...', "\n";
    &run_command ([split (/\s+/, $editor), $file]);
  }

  # Read the edited file, and extract user log message from it
  open FILE, '<', $file or croak 'Cannot open ', $file, ' (', $!, '), abort';
  my (@log);
  my $in_auto    = 0;
  my $in_ignored = 0;

  while (<FILE>) {
    if (index ($_, $auto_line) == 0) {
      $in_auto = 1;

    } elsif (index ($_, $ignore_line) == 0) {
      $in_ignored = 1;
      last;

    } else {
      next if $in_auto;
      push @log, $_;
    }
  }

  close FILE;

  # Ensure auto and ignored lines are not altered
  e_report 'Error: the line "', $auto_line, '" has been altered, abort.'
    if @auto and not $in_auto;

  e_report 'Error: the line "', $ignore_line, '" has been altered, abort.'
    if not $in_ignored;

  # Check for empty commit log
  e_report 'Error: log message unchanged or not specified, abort.'
    if join (' ', (@log, @auto)) =~ /^\s*$/;

  # Echo the commit message to standard output
  my $separator = '-' x 80 . "\n";
  print 'Commit message is as follows:', "\n";
  print $separator, @log, @auto, $ignore_line, "\n", @ignore, $separator;

  open FILE, '>', $file or croak 'Cannot open ', $file, ' (', $!, '), abort';
  print FILE @log, @auto;
  close FILE or croak 'Cannot close ', $file, ' (', $!, '), abort';

  # Reset the array for the user specified log message
  $self->user_mesg (@log);

  return $file;
}

# ------------------------------------------------------------------------------

1;

__END__
