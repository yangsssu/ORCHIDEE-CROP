#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::CfgFile
#
# DESCRIPTION
#   This class is used for reading and writing FCM config files. A FCM config
#   file is a line-based text file that provides information on how to perform
#   a particular task using the FCM system.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::CfgFile;

# Standard pragma
use warnings;
use strict;

# Standard modules
use Carp;
use File::Basename;
use File::Path;
use File::Spec;
use File::Spec::Functions;

# FCM component modules
use Fcm::Util;

# Local module variables
my $expand_type   = 'bld|ext'; # config file type that needs variable expansions

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $cfgfile = Fcm::CfgFile->new (CONFIG=> $config, SRC => $src, TYPE => $type);
#
# DESCRIPTION
#   This method constructs a new instance of the Fcm::CfgFile class.
#
# ARGUMENTS
#   CONFIG - reference to a Fcm::Config instance
#   SRC    - configuration file source
#   TYPE   - type of expected configuration file
# ------------------------------------------------------------------------------

sub new {
  my $this  = shift;
  my %args  = @_;
  my $class = ref $this || $this;

  my $self = {
    CONFIG     => exists $args{CONFIG} ? $args{CONFIG} : &main::cfg,
    SRC        => exists $args{SRC}    ? $args{SRC}    : undef,
    TYPE       => exists $args{TYPE}   ? $args{TYPE}   : undef,

    # Version of the configuration file
    VERSION    => undef,

    # List of references to hash tables for each line in the file
    LINES      => [],

    # Actual source of configuration file
    ACTUAL_SRC => undef,
    PEGREV     => undef,
  };
  bless $self, $class;
  return $self;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config = $cfgfile->config;
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
#   $src = $cfgfile->src ();
#   $cfgfile->src ($src);
#
# DESCRIPTION
#   This method returns the specified source of the configuration file. If an
#   argument is specified, the source of the configuration file is modified to
#   the value of the argument.
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
#   $src = $cfgfile->actual_src ();
#
# DESCRIPTION
#   This method returns the actual source of the configuration file. If an
#   argument is specified, the source of the configuration file is modified to
#   the value of the argument.
# ------------------------------------------------------------------------------

sub actual_src {
  my $self = shift;

  return $self->{ACTUAL_SRC};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rev = $cfgfile->pegrev ();
#
# DESCRIPTION
#   This method returns the peg revision of the configuration file.
# ------------------------------------------------------------------------------

sub pegrev {
  my $self = shift;

  return $self->{PEGREV};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $type = $cfgfile->type ();
#   $cfgfile->type ($type);
#
# DESCRIPTION
#   This method returns the configuration file type. If an argument is
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
#   $version = $cfgfile->version ();
#
# DESCRIPTION
#   This method returns the version of the configuration file.
# ------------------------------------------------------------------------------

sub version {
  my $self = shift;

  return $self->{VERSION};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @lines = $cfgfile->lines ();
#
# DESCRIPTION
#   This method returns an array containing all "lines" in the configuration
#   file. Each "line" is a reference to a hash table with the following keys:
#
#   SRC     - the source of the configuration file
#   NUMBER  - the line number in the source
#   LABEL   - the label of the of the configuration line
#   VALUE   - the value of the configuration line
#   COMMENT - comment in the configuration line
# ------------------------------------------------------------------------------

sub lines {
  my $self = shift;

  return @{ $self->{LINES} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $line = $cfgfile->line ($line_number);
#
# DESCRIPTION
#   This method return a "line" at $line_number in the configuration file. A
#   "line" is a reference to a hash table with the following keys:
#
#   SRC     - the source of the configuration file
#   NUMBER  - the line number in the source
#   LABEL   - the label of the of the configuration line
#   VALUE   - the value of the configuration line
#   COMMENT - comment in the configuration line
# ------------------------------------------------------------------------------

sub line {
  my $self     = shift;
  my $line_num = shift;

  if (exists $self->{LINES}[$line_num]) {
    return $self->{LINES}[$line_num]; # returns a ref to a label:value pair hash

  } else {
    return undef;
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $cfgfile->add_line (
#     LABEL   => $label,
#     VALUE   => $value,
#     COMMENT => $comment,
#     SRC     => $src,
#     NUMBER  => $line_number
#   );
#
# DESCRIPTION
#   This method adds a "line" to the configuration file. LABEL is the
#   configuration line label. VALUE is the configuration line value. COMMENT
#   is the comment in the line. VALUE should only be specified if LABEL is
#   specified. COMMENT can be specified without LABEL. In such case, the whole
#   line is a comment line. A blank line is inserted if no argument is
#   specified. SRC can be specified to indicate the name of the source file
#   from which this line is obtained. If not specified, the source file of the
#   current configuration file is used. NUMBER can be specified to indicate
#   the line number of the source file from which this line is obtained.
# ------------------------------------------------------------------------------

sub add_line {
  my $self = shift;
  my %args = @_;

  my $line = {
    SRC     => exists $args{SRC    } ? $args{SRC    } : $self->actual_src,
    NUMBER  => exists $args{NUMBER } ? $args{NUMBER } : 0,
    LABEL   => exists $args{LABEL  } ? $args{LABEL  } : '',
    VALUE   => exists $args{VALUE  } ? $args{VALUE  } : '',
    COMMENT => exists $args{COMMENT} ? $args{COMMENT} : '',
  };

  push @{ $self->{LINES} }, $line;

  return $line;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $cfgfile->add_comment_block ($line1, [$line2, ...]);
#
# DESCRIPTION
#   This method adds a comment block to the configuration file. Each argument
#   represents a line in the comment block.
# ------------------------------------------------------------------------------

sub add_comment_block {
  my $self = shift;

  $self->add_line (COMMENT => '-' x 78,);

  while (my $line = shift @_) {
    $self->add_line (COMMENT => $line,)
  }

  $self->add_line (COMMENT => '-' x 78,);
  $self->add_line;

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $cfgfile->add_header ();
#
# DESCRIPTION
#   This method adds a header to the configuration file, with its type and
#   type version. It returns 1 on success.
# ------------------------------------------------------------------------------

sub add_header {
  my $self = shift;

  return undef unless $self->{TYPE};

  $self->{VERSION} = $self->config->setting ('CFG_VERSION', uc ($self->{TYPE}))
    if not $self->{VERSION};

  $self->add_comment_block ('File header');

  $self->add_line (
    LABEL => $self->config->setting (qw/CFG_LABEL CFGFILE TYPE/),
    VALUE => $self->{TYPE},
  );

  $self->add_line (
    LABEL => $self->config->setting (qw/CFG_LABEL CFGFILE VERSION/),
    VALUE => $self->{VERSION},
  );

  $self->add_line;

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $exist = $cfg->label_exists ($label);
#   @lines = $cfg->label_exists ($label);
#
# DESCRIPTION
#   This method returns the (number of) "lines" with their LABEL matching the
#   argument $label.
# ------------------------------------------------------------------------------

sub label_exists {
  my $self  = shift;
  my $label = shift;

  return grep {$_->{LABEL} eq $label} @{ $self->{LINES} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $mtime = $cfgfile->mtime ();
#
# DESCRIPTION
#   This method returns the modified time of the configuration file source.
# ------------------------------------------------------------------------------

sub mtime {
  my $self  = shift;
  my $mtime = undef;

  if (-f $self->{SRC}) {
    $mtime = (stat $self->{SRC})[9];
  }

  return $mtime;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $read = $cfgfile->read_cfg ();
#
# DESCRIPTION
#   This method reads the current configuration file. It returns the number of
#   lines read from the config file, or "undef" if it fails. The result is
#   placed in the LINES array of the current instance, and can be accessed via
#   the "lines" method.
# ------------------------------------------------------------------------------

sub read_cfg {
  my $self = shift;

  my @lines = $self->_get_cfg_lines;

  # List of CFG labels
  my %cfg_labels = %{ $self->config->setting ('CFG_LABEL') };

  # List of CFG types that need INC declarations expansion
  my %exp_inc    = ();
  for (split (/,/, $self->config->setting ('CFG_EXP_INC'))) {
    $exp_inc{uc ($_)} = 1;
  }

  # List of CFG labels that are reserved keywords
  my %cfg_keywords = ();
  for (split (/,/, $self->config->setting ('CFG_KEYWORD'))) {
    $cfg_keywords{$cfg_labels{$_}} = 1;
  }

  # Loop each line, to separate lines into label : value pairs
  my $cont = undef;
  my $here = undef;
  for my $line_num (1 .. @lines) {
    my $line = $lines[$line_num - 1];
    chomp $line;

    my $label   = '';
    my $value   = '';
    my $comment = '';

    # If this line is a continuation, set $start to point to the line that
    # starts this continuation. Otherwise, set $start to undef
    my $start   = defined ($cont) ? $self->line ($cont) : undef;

    if ($line =~ /^(\s*#.*)$/) { # comment line
      $comment = $1;

    } elsif ($line =~ /\S/) {    # non-blank line
      if (defined $cont) {
        # Previous line has a continuation mark
        $value = $line;

        # Separate value and comment
        if ($value =~ s/((?:\s+|^)#\s+.*)$//) {
          $comment = $1;
        }

        # Remove leading spaces
        $value =~ s/^\s*\\?//;

        # Expand environment variables
        $value = $self->_expand_variable ($value, 1) if $value;

        # Expand internal variables
        $value = $self->_expand_variable ($value) if $value;

        # Get "line" that begins the current continuation
        ($start->{VALUE} .= $value) =~ s/\\$//;

      } else {
        # Previous line does not have a continuation mark
        if ($line =~ /^\s*(\S+)(?:\s+(.*))?$/) {
          # Check line contains a valid label:value pair
          $label = $1;
          $value = defined ($2) ? $2 : '';

          # Separate value and comment
          if ($value =~ s/((?:\s+|^)#\s+.*)$//) {
            $comment = $1;
          }

          # Remove trailing spaces
          $value =~ s/\s+$//;

          # Value begins with $HERE?
          $here  = ($value =~ /\$\{?HERE\}?(?:[^A-Z_]|$)/);

          # Expand environment variables
          $value = $self->_expand_variable ($value, 1) if $value;

          # Expand internal variables
          $value = $self->_expand_variable ($value) if $value;
        }
      }

      # Determine whether current line ends with a continuation mark
      if ($value =~ s/\\$//) {
        $cont = scalar ($self->lines) unless $cont;

      } else {
        $cont = undef;
      }
    }

    if (exists $exp_inc{uc ($self->type)} and
        uc ($start ? $start->{LABEL} : $label) eq $cfg_labels{INC} and
        not defined $cont) {
      # Current configuration file requires expansion of INC declarations
      # The start/current line is an INC declaration
      # The current line is not a continuation or is the end of the continuation

      # Get lines from an "include" configuration file
      my $src = ($start ? $start->{VALUE} : $value);
      $src   .= '@' . $self->pegrev if $here and $self->pegrev;

      if ($src) {
        # Invoke a new instance to read the source
        my $cfg = Fcm::CfgFile->new (
          SRC    => expand_tilde ($src),
          TYPE   => $self->type,
          CONFIG => $self->config,
        );

        $cfg->read_cfg;

        # Add lines to the "LINES" array in the current configuration file
        $comment = 'INC ' . $src . ' ';
        $self->add_line (
          COMMENT => $comment . '# Start',
          NUMBER  => ($start ? $start->{NUMBER} : $line_num),
        );
        $self->add_line (%{ $_ }) for (($cfg->lines));
        $self->add_line (COMMENT => $comment . '# End');

      } else {
        $self->add_line (NUMBER  => $line_num);
        w_report 'Warning: ', $self->actual_src, ': line ', $line_num,
                 ': empty INC declaration.' if $self->config->verbose > 2;
      }

    } else {
      # Push label:value pair into "LINES" array
      $self->add_line (
        LABEL   => $label,
        VALUE   => ($label ? $value : ''),
        COMMENT => $comment,
        NUMBER  => $line_num,
      );
    }

    next if defined $cont; # current line not a continuation

    my $slabel = ($start ? $start->{LABEL} : $label);
    my $svalue = ($start ? $start->{VALUE} : $value);
    next unless $slabel;

    # Check config file type and version
    if (uc ($slabel) eq $cfg_labels{CFGFILE}{TYPE}) {
      $self->type ($svalue);

    } elsif (uc ($slabel) eq $cfg_labels{CFGFILE}{VERSION}) {
      $self->version ($svalue);
    }

    # Set internal variable
    $slabel =~ s/^\%//; # Remove leading "%" from label

    $self->config->assign_variable (
      LABEL => $slabel,
      VALUE => $svalue,
    ) unless exists $cfg_keywords{$slabel};
  }

  return $self->lines;

}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = $cfgfile->print_cfg ($file);
#
# DESCRIPTION
#   This method prints the content of current configuration file. If no
#   argument is specified, it prints output to the standard output. If $file
#   is specified, and is a writable file name, the output is sent to the file.
#   If the file already exists, its content is compared to the current output.
#   Nothing will be written if the content is unchanged. Otherwise, for typed
#   configuration files, the existing file is renamed using a prefix that
#   contains its last modified time. The method returns 1 if there is no
#   error.
# ------------------------------------------------------------------------------

sub print_cfg {
  my $self = shift;
  my $file = shift;

  # Count maximum number of characters in the labels, (for pretty printing)
  my $max_label_len = 0;
  for my $line (@{ $self->{LINES} }) {
    next unless $line->{LABEL};
    my $label_len  = length $line->{LABEL};
    $max_label_len = $label_len if $label_len > $max_label_len;
  }

  # Output string
  my $out = '';

  # Append each line of the config file to the output string
  for my $line (@{ $self->{LINES} }) {
    my $label   = $line->{LABEL};
    my $value   = $line->{VALUE};
    my $comment = $line->{COMMENT};

    if ($label) {
      # Line up label/value for pretty printing
      $label   = $label . ' ' x ($max_label_len - length ($label));
      $comment =~ s/^\s+/ / if $comment;

      $out .= $label;
      $out .= ' ' . $value if defined $value;

    } else {
      # Make sure comments begin with a "#"
      $comment = '# ' . $comment if $comment and $comment !~ /^\s*($|#)/;
      $comment =~ s/^\s*//;
    }

    $out .= $comment if $comment;
    $out .= "\n";
  }

  if ($out) {

    my $old_select = select;

    # Open file if necessary
    if ($file) {
      # Make sure the host directory exists and is writable
      my $dirname = dirname $file;
      if (not -d $dirname) {
        print 'Make directory: ', $dirname, "\n" if $self->config->verbose;
        mkpath $dirname;
      }
      croak 'Cannot write to config file directory: "', $dirname, '", abort'
        unless -d $dirname and -w $dirname;

      # If config file already exists, make sure it is writable
      if (-f $file) {
        if (-r $file) {
          # Read old config file to see if content has changed
          open IN, '<', $file;
          my $in_lines = '';
          while (my $line = <IN>) {
            $in_lines .= $line;
          }
          close IN;

          # Return if content is up-to-date
          if ($in_lines eq $out) {
            print 'No change in ', lc ($self->{TYPE}), ' cfg: ', $file,
                  "\n" if $self->config->verbose > 1 and $self->{TYPE};
            return;
          }
        }

        if (-w $file) {
          if ($self->{TYPE}) {
            # Existing config file writable, rename it using its time stamp
            my $mtime = (stat $file)[9];
            my ($sec, $min, $hour, $mday, $mon, $year) = (gmtime $mtime)[0 .. 5];
            my $timestamp = sprintf '%4d%2.2d%2.2d_%2.2d%2.2d%2.2d_',
                            $year + 1900, $mon + 1, $mday, $hour, $min, $sec;
            my $oldfile   = catfile $dirname, $timestamp . basename ($file);
            rename $file, $oldfile;
            print 'Renamed existing ', lc ($self->{TYPE}), ' cfg: ',
                  $oldfile, "\n" if $self->config->verbose > 1;
          }
        } else {
          # Existing config file not writable, throw an error
          croak 'Config file "', $file, '" not writable, abort';
        }
      }

      # Open file and select file handle
      open OUT, '>', $file
        or croak 'Cannot write to config file "', $file, '" (', $!, '), abort';
      select OUT;
    }

    # Print output
    print $out;

    # Close file if necessary
    if ($file) {
      select $old_select;
      close OUT;

      if ($self->{TYPE} and $self->config->verbose > 1) {
        print 'Generated ', lc ($self->{TYPE}), ' cfg: ', $file, "\n";
      } elsif ($self->config->verbose > 2) {
        print 'Generated cfg: ', $file, "\n";
      }
    }

  } else {

    # Warn if nothing to print
    my $warning = 'Empty configuration';
    $warning   .= ' - nothing written to file: "' . $file . '"' if $file;
    carp $warning if $self->{TYPE};

  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @lines = $self->_get_cfg_lines ();
#
# DESCRIPTION
#   This internal method reads from a configuration file residing in a
#   Subversion repository or in the normal file system.
# ------------------------------------------------------------------------------

sub _get_cfg_lines {
  my $self  = shift;
  my @lines = ();

  my $verbose = $self->config->verbose;

  # Expand URL keywords if necessary
  {
    my $src = expand_url_keyword (URL => $self->src, CFG => $self->config);
    $self->src ($src) if $src ne $self->src;
  }

  if (&is_url ($self->src)) {
    # Config file resides in a SVN repository
    # --------------------------------------------------------------------------
    # Set URL source and version
    my $src = $self->src;
    my $rev = 'HEAD';

    # Extract version from source if it exists
    if ($src =~ s/@(.+)$//) {
      $rev = $1;
    }

    # Expand revision keyword, if required
    $rev = expand_rev_keyword (REV => $rev, URL => $src, HEAD => 1);

    # Check whether URL is a config file
    my $rc;
    my @cmd = (qw/svn cat/, $src . '@' . $rev);
    @lines = &run_command (
      \@cmd, METHOD => 'qx', DEVNULL => 1, RC => \$rc, ERROR => 'ignore',
    );

    # Error in "svn cat" command
    if ($rc) {
      # See whether specified config file is a known type
      my %cfgname = %{ $self->config->setting ('CFG_NAME') };
      my $key     = uc $self->type;
      my $file    = exists $cfgname{$key} ? $cfgname{$key} : '';

      # If config file is a known type, specified URL may be a directory
      if ($file) {
        # Check whether a config file with a default name exists in the URL
        my $sep  = $self->config->setting (qw/MISC DIR_SEPARATOR/);
        my $path = $src . $sep . $file;
        my @cmd  = (qw/svn cat/, $path . '@' . $rev);

        @lines = &run_command (
          \@cmd, METHOD => 'qx', DEVNULL => 1, RC => \$rc, ERROR => 'ignore',
        );

        # Check whether a config file with a default name exists under the "cfg"
        # sub-directory of the URL
        if ($rc) {
          my $cfgdir = $self->config->setting (qw/DIR CFG/);
          $path   = $src . $sep . $cfgdir . $sep . $file;
          my @cmd = (qw/svn cat/, $path . '@' . $rev);

          @lines  = &run_command (
            \@cmd, METHOD => 'qx', DEVNULL => 1, RC => \$rc, ERROR => 'ignore',
          );
        }

        $src = $path unless $rc;
      }
    }

    if ($rc) {
      # Error in "svn cat"
      croak 'Unable to locate config file from "', $self->src, '", abort';

    } else {
      # Print diagnostic, if necessary
      if ($verbose and $self->type and $self->type =~ /$expand_type/) {
        print '# Config file (', $self->type, '): ', $src;
        print '@', $rev if $rev;
        print "\n";
      }
    }

    # Record the actual source location
    $self->{PEGREV    } = $rev;
    $self->{ACTUAL_SRC} = $src;

  } else {
    # Config file resides in the normal file system
    # --------------------------------------------------------------------------
    my $src = $self->src;

    if (-d $src) { # Source is a directory
      croak 'Config file "', $src, '" is a directory, abort' if not $self->type;

      # Get name of the config file by looking at the type
      my %cfgname = %{ $self->config->setting ('CFG_NAME') };
      my $key     = uc $self->type;
      my $file    = exists $cfgname{$key} ? $cfgname{$key} : '';

      if ($file) {
        my $cfgdir = $self->config->setting (qw/DIR CFG/);

        # Check whether a config file with a default name exists in the
        # specified path, then check whether a config file with a default name
        # exists under the "cfg" sub-directory of the specified path
        if (-f catfile $self->src, $file) {
          $src = catfile $self->src, $file;

        } elsif (-f catfile ($self->src, $cfgdir, $file)) {
          $src = catfile $self->src, $cfgdir, $file;

        } else {
          croak 'Unable to locate config file from "', $self->src, '", abort';
        }

      } else {
        croak 'Unknown config file type "', $self->type, '", abort';
      }
    }

    if (-r $src) {
      open FILE, '<', $src;
      print '# Config file (', $self->type, '): ', $src, "\n"
        if $verbose and $self->type and $self->type =~ /$expand_type/;

      @lines = readline 'FILE';
      close FILE;

    } else {
      croak 'Unable to read config file "', $src, '", abort';
    }

    # Record the actual source location
    $self->{ACTUAL_SRC} = $src;
  }

  return @lines;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $string = $self->_expand_variable ($string[, $env]);
#
# DESCRIPTION
#   This internal method expands variables in $string. If $env is specified
#   and is true, it expands environment variables. Otherwise, it expands
#   local variables.
# ------------------------------------------------------------------------------

sub _expand_variable {
  my ($self, $string, $env) = @_;

  # Pattern for environment/local variable
  my $pattern = $env ? '\$\{?([A-Z][A-Z0-9_]+)\}?' : '%\{?([\w:]+)\}?';

  while ($string and $string =~ /$pattern/) {
    my $var_label = $1; # variable label

    # Get variable value from environment or local configuration
    my $variable = $env
                   ? (exists $ENV{$var_label} ? $ENV{$var_label} : undef)
                   : $self->config->variable ($var_label);

    $variable = dirname ($self->actual_src)
      if $env and $var_label eq 'HERE' and not defined $variable;

    # Substitute match with value of variable
    if (defined $variable) {
      $string =~ s/$pattern/$variable/;

    } else {
      w_report 'Warning: ', $self->actual_src, ': variable "',
               ($env ? '$' : '%'), $var_label, '" not expanded.';
      last;
    }
  }

  return $string;
}

1;

__END__
