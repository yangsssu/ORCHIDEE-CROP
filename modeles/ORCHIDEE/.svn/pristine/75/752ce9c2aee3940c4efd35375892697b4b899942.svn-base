#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::Util
#
# DESCRIPTION
#   This is a package of misc utilities used by the FCM command.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::Util;

# Standard pragma
use warnings;
use strict;

# Exports
our (@ISA, @EXPORT, @EXPORT_OK);

sub expand_rev_keyword;
sub expand_tilde;
sub expand_url_keyword;
sub e_report;
sub find_srcdir;
sub find_file_in_path;
sub get_browser_url;
sub get_command_string;
sub get_rev_of_wc;
sub get_rev_keyword;
sub get_url_of_wc;
sub get_url_keyword;
sub get_wct;
sub is_url;
sub is_wc;
sub print_command;
sub run_command;
sub svn_date;
sub touch_file;
sub w_report;

require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(
  expand_rev_keyword
  expand_tilde
  expand_url_keyword
  e_report
  find_srcdir
  find_file_in_path
  get_browser_url
  get_command_string
  get_rev_of_wc
  get_rev_keyword
  get_url_of_wc
  get_url_keyword
  get_wct
  is_url
  is_wc
  print_command
  run_command
  svn_date
  touch_file
  w_report
);

# Standard modules
use Carp;
use Cwd;
use File::Basename;
use File::Find;
use File::Path;
use File::Spec;
use POSIX qw/strftime/;

# FCM component modules
use Fcm::Timer;

# ------------------------------------------------------------------------------

# Module level variables
my %svn_info       = (); # "svn info" log, (key1 = path,
                         # key2 = URL, Revision, Last Changed Rev)

# ------------------------------------------------------------------------------
# SYNOPSIS
#   %srcdir = &Fcm::Util::find_srcdir ($topdir, $toppck, $join);
#
# DESCRIPTION
#   Search $topdir for sub-directories containing regular files. Returns a hash
#   with each key/value pair assigned to a unique name of the source directory
#   and the location of the source directory. If $toppck is set the name of
#   each source directory will be prefixed with this package name, and the
#   search may include the $topdir in the result. If $join is set, the name of
#   the sub-package will use $join as the delimiter of packages. Otherwise, the
#   default double underscore '__' will be used.  Please note that all
#   directories beginning with a ".", i.e. hidden directories, are ignored.
# ------------------------------------------------------------------------------

sub find_srcdir {
  (my $topdir, my $toppck, my $join) = @_;
  $join = defined ($join) ? $join : '__';

  my @dirs = ();

  # Locate all source directories containing regular files
  if (-d $topdir) {
    find (
      sub {
        my $dir = $File::Find::name;
        return 0 if $dir eq $topdir and not $toppck;

        if (-d $dir) {
          # Ignore sub-directories with names beginning with .
          if ($dir ne $topdir) {
            my $subdir = substr ($dir, length ($topdir) + 1);
            return 0 if grep {m/^\./} File::Spec->splitdir ($subdir);
          }

          # Read contents of directory
          opendir DIR, $dir;
          my @files = readdir 'DIR';
          closedir DIR;

          # Check if the directory contains one or more source file
          my $contain_src;
          for my $file (@files) {
            next if $file =~ /^\./; # ignore hidden file

            if (-f File::Spec->catfile ($dir, $file)) {
              $contain_src = 1;
              last;
            }
          }

          push @dirs, $dir if $contain_src;
          return 1;

        } else {
          return 0;
        }
      },

      $topdir,
    );
  }

  # String length of src directory name
  my $topdir_len = length $topdir;

  # Assign new source directories to current build
  my @pck    = $toppck ? split (/$join/, $toppck) : ();
  my %srcdir = ();
  for my $dir (@dirs) {
    my $name = ($dir eq $topdir) ? '' : substr $dir, $topdir_len + 1;
    my @path = File::Spec->splitdir ($name);
    my $key  = join $join, (@pck, @path);

    $srcdir{$key} = $dir;
  }

  return %srcdir;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   %srcdir = &Fcm::Util::find_file_in_path ($file, \@path);
#
# DESCRIPTION
#   Search $file in @path. Returns the full path of the $file if it is found
#   in @path. Returns "undef" if $file is not found in @path.
# ------------------------------------------------------------------------------

sub find_file_in_path {
  my ($file, $path) = @_;

  for my $dir (@$path) {
    my $full_file = File::Spec->catfile ($dir, $file);
    return $full_file if -e $full_file;
  }

  return undef;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $expanded_path = &Fcm::Util::expand_tilde ($path);
#
# DESCRIPTION
#   Returns an expanded path if $path is a path that begins with a tilde (~).
# ------------------------------------------------------------------------------

sub expand_tilde {
  my $file = $_[0];

  $file =~ s#^~([^/]*)#$1 ? (getpwnam $1)[7] : ($ENV{HOME} || $ENV{LOGDIR})#ex;

  return $file;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = &Fcm::Util::touch_file ($file);
#
# DESCRIPTION
#   Touch $file if it exists. Create $file if it does not exist. Return 1 for
#   success or 0 otherwise.
# ------------------------------------------------------------------------------

sub touch_file {
  my $file = $_[0];
  my $rc   = 1;

  if (-e $file) {
    my $now = time;
    $rc = utime $now, $now, $file;

  } else {
    mkpath dirname ($file) unless -d dirname ($file);

    $rc = open FILE, '>', $file;
    $rc = close FILE if $rc;
  }

  return $rc;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $new_url = &Fcm::Util::expand_url_keyword (URL => $url[, CFG => $cfg]);
#
# DESCRIPTION
#   Expand URL if its begins with a pre-defined pattern followed by a keyword
#   that can be found in the setting of CFG. If URL is a genuine URL, the
#   function also attempts to expand any . or .. in the path. If CFG is not
#   set, it defaults to &main::cfg.
# ------------------------------------------------------------------------------

sub expand_url_keyword {
  my %args = @_;
  my $url  = $args{URL};
  my $cfg  = exists $args{CFG} ? $args{CFG} : &main::cfg;

  # Prefix for URL keyword
  my $prefix = $cfg->setting (qw/MISC EXPURL_PREFIX/);

  # Pattern for URL keyword
  my $pattern = '^' . $prefix . '([^/]+)';

  # Standard suffix for URL keyword
  my %suffix_value = (tr => 'trunk', br => 'branches', tg => 'tags');

  # URL matches pattern?
  if ($url =~ /$pattern/) {
    my $keyword = $1;

    # Determine whether keyword is registered.
    my $keyval = $cfg->setting ('URL', uc ($keyword));

    if ((not $keyval) and $keyword =~ s/[-_](tr|br|tg)$//) {
      # Keyword is not registered, but it matches a standard suffix
      my $suffix = $suffix_value{$1};

      $keyval = $cfg->setting ('URL', uc ($keyword)) . '/' . $suffix
        if $cfg->setting ('URL', uc ($keyword));
    }

    # Expand if keyword is registered
    $url =~ s/$pattern/$keyval/ if $keyval;
  }

  # Expand . and ..
  if (&is_url ($url)) {
    while ($url =~ s#/+\.(?:/+|$)#/#g) {next}
    while ($url =~ s#/+[^/]+/+\.\.(?:/+|$)#/#g) {next}
  }

  return $url;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $string = &Fcm::Util::get_url_keyword (URL => $url[, CFG => $cfg]);
#
# DESCRIPTION
#   Return a FCM URL keyword if URL matches a registered project URL or undef
#   otherwise. If CFG is not set, it defaults to &main::cfg.
# ------------------------------------------------------------------------------

sub get_url_keyword {
  my %args = @_;
  my $url  = $args{URL};
  my $cfg  = exists $args{CFG} ? $args{CFG} : &main::cfg;

  my $return;

  for my $key (%{ $cfg->setting ('URL') }) {
    my $value = $cfg->setting ('URL', $key);
    next unless defined $value;
    next unless $url =~ s#^$value(?:/+|$)##;

    $return = $cfg->setting (qw/MISC EXPURL_PREFIX/) . $key .
              ($url ? '/' . $url : '');
    last;
  }

  return $return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $new_rev = &Fcm::Util::expand_rev_keyword (
#     REV  => $rev,
#     URL  => $url,
#    [HEAD => $flag,]
#    [CFG  => $cfg,]
#  );
#
# DESCRIPTION
#   Expand REV if URL is a known URL in CFG setting and REV matches a revision
#   keyword of this URL, or if REV is "HEAD". SVN revision numbers, date and
#   other keywords are ignored. HEAD should only be specified if REV has the
#   value "HEAD". If HEAD is specified and is true, the return value of the
#   function will be the operative revision number of the HEAD revision.
#   Otherwise, the last commit revision will be returned. If CFG is not set,
#   it defaults to &main::cfg.
# ------------------------------------------------------------------------------

sub expand_rev_keyword {
  my %args = @_;
  my $rev  = $args{REV};
  my $url  = $args{URL};
  my $head = exists $args{HEAD} ? $args{HEAD} : undef;
  my $cfg  = exists $args{CFG } ? $args{CFG } : &main::cfg;

  if (uc ($rev) eq 'HEAD') {
    # Expand HEAD revision
    &_invoke_svn_info (PATH => $url, CFG => $cfg) unless exists $svn_info{$url};
    my $expanded_rev = $head
                     ? $svn_info{$url}{Revision}
                     : $svn_info{$url}{'Last Changed Rev'};

    &w_report ($url, ': cannot determine HEAD revision.')
      if $cfg->verbose > 1 and not $expanded_rev;

    $rev = $expanded_rev if $expanded_rev;

  } elsif ($rev !~ /^(?:\d+|BASE|COMMITTED|PREV|\{.+\})$/i) {
    # Expand revision keyword, if required

    # Get configuration settings
    my %keywords  = %{ $cfg->setting (qw/REVISION/) };
    my $separator = $cfg->setting (qw/MISC DIR_SEPARATOR/);

    my $name      = '';

    # Find out whether URL matches a registered repository
    for my $keyword (keys %keywords) {
      my $repos = $cfg->setting ('URL', uc ($keyword));
      next unless $repos;

      if ($url =~ m#^$repos(?:$separator|$)#) {
        $name = $keyword;
        last;
      }
    }

    # If revision keyword exists for the registered repository, expand it
    if ($name and exists $keywords{$name}{uc ($rev)}) {
      $rev = $keywords{$name}{uc ($rev)};

    } else {
      &e_report (
        $rev, ': revision keyword not found for ', $url,
        ' in FCM configuration file, abort.',
      );
    }
  }

  return $rev;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $keyword = Fcm::Util::get_rev_keyword (
#     REV => $rev,
#     URL => $url,
#    [CFG => $cfg,]
#  );
#
# DESCRIPTION
#   Returns a revision keyword if URL is a known URL in CFG setting and REV is
#   a revision number that matches a revision keyword of this URL. Otherwise,
#   it returns REV unchanged. If CFG is not set, it defaults to &main::cfg.
# ------------------------------------------------------------------------------

sub get_rev_keyword {
  my %args = @_;
  my $rev  = $args{REV};
  my $url  = $args{URL};
  my $cfg  = exists $args{CFG} ? $args{CFG} : &main::cfg;

  if ($rev =~ /^\d+$/) {
    # Get revision keyword, if REV is a revision number

    # Get configuration settings
    my %keywords  = %{ $cfg->setting (qw/REVISION/) };
    my $separator = $cfg->setting (qw/MISC DIR_SEPARATOR/);

    my $name      = '';

    # Find out whether URL matches a registered repository
    for my $keyword (keys %keywords) {
      my $repos = $cfg->setting ('URL', uc ($keyword));
      next unless $repos;

      if ($url =~ m#^$repos(?:$separator|$)#) {
        $name = $keyword;
        last;
      }
    }

    # If revision keyword for REV exists for the registered repository, get it
    if ($name and exists $keywords{$name} and ref $keywords{$name} eq 'HASH') {
      for my $key (keys %{ $keywords{$name} }) {
        if ($rev eq $keywords{$name}{$key}) {
          $rev = $key;
          last;
        }
      }
    }
  }

  return $rev;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $browser_url = Fcm::Util::get_browser_url (
#     URL => $url,
#    [CFG => $cfg,]
#  );
#
# DESCRIPTION
#   Returns a web address for browsing URL from Trac if URL is a known URL in
#   CFG setting, and that it is a matching web address. Otherwise, it returns
#   "undef". If CFG is not set, it defaults to &main::cfg.
# ------------------------------------------------------------------------------

sub get_browser_url {
  my %args        = @_;
  my $url         = $args{URL};
  my $cfg         = exists $args{CFG} ? $args{CFG} : &main::cfg;
  my $browser_url = undef;

  # Get configuration settings
  my %keywords  = %{ $cfg->setting (qw/TRAC/) };
  my $separator = $cfg->setting (qw/MISC DIR_SEPARATOR/);

  my $name  = '';
  my $trail = '';

  # Find out whether URL matches a registered repository
  for my $keyword (keys %keywords) {
    my $repos = $cfg->setting ('URL', uc ($keyword));
    next unless $repos;

    if ($url =~ m#^$repos(?:$separator(.*$)|$)#) {
      $name  = $keyword;
      $trail = $1 if $1;
      last;
    }
  }

  # If TRAC web address exists for the registered repository, get it
  if ($name and exists $keywords{$name}) {
    $browser_url  = $keywords{$name};
    $browser_url .= $separator . $trail if $trail;
  }

  return $browser_url;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $flag = &is_wc ([$path]);
#
# DESCRIPTION
#   Returns true if current working directory (or $path) is a Subversion
#   working copy.
# ------------------------------------------------------------------------------

sub is_wc {
  my $path = @_ ? $_[0] : cwd ();

  if (-d $path) {
    return (-e File::Spec->catfile ($path, qw/.svn format/)) ? 1 : 0;

  } elsif (-f $path) {
    return (-e File::Spec->catfile (dirname ($path), qw/.svn format/)) ? 1 : 0;

  } else {
    return 0;
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $flag = &is_url ($url);
#
# DESCRIPTION
#   Returns true if $url is a URL.
# ------------------------------------------------------------------------------

sub is_url {
  # This should handle URL beginning with svn://, http:// and svn+ssh://
  return ($_[0] =~ m#^[\+\w]+://#);
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $string = &get_wct ([$dir]);
#
# DESCRIPTION
#   If current working directory (or $dir) is a Subversion working copy,
#   returns the top directory of this working copy; otherwise returns an empty
#   string.
# ------------------------------------------------------------------------------

sub get_wct {
  my $dir = @_ ? $_[0] : cwd ();

  return '' if not &is_wc ($dir);

  my $updir = dirname $dir;
  while (&is_wc ($updir)) {
    $dir   = $updir;
    $updir = dirname $dir;
    last if $updir eq $dir;
  }

  return $dir;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $string = &get_url_of_wc ([$path[, $refresh]]);
#
# DESCRIPTION
#   If current working directory (or $path) is a Subversion working copy,
#   returns the URL of the associated Subversion repository; otherwise returns
#   an empty string. If $refresh is specified, do not use the cached
#   information.
# ------------------------------------------------------------------------------

sub get_url_of_wc {
  my $path    = @_ ? $_[0] : cwd ();
  my $refresh = exists $_[1] ? $_[1] : 0;
  my $url  = '';

  if (&is_wc ($path)) {
    delete $svn_info{$path} if $refresh;
    &_invoke_svn_info (PATH => $path) unless exists $svn_info{$path};
    $url = $svn_info{$path}{URL};
  }

  return $url;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &_invoke_svn_info (PATH => $path, [CFG => $cfg]);
#
# DESCRIPTION
#   The function is internal to this module. It invokes "svn info" on $path to
#   gather information on URL, Revision and Last Changed Rev. The information
#   is stored in a hash table at the module level, so that the information can
#   be re-used. If CFG is not set, it defaults to &main::cfg.
# ------------------------------------------------------------------------------

sub _invoke_svn_info {
  my %args = @_;
  my $path = $args{PATH};
  my $cfg  = exists $args{CFG} ? $args{CFG} : &main::cfg;

  return if exists $svn_info{$path};

  # Invoke "svn info" command
  my @info = &run_command (
    [qw/svn info/, $path],
    PRINT => $cfg->verbose > 2, METHOD => 'qx', DEVNULL => 1, ERROR => 'ignore',
  );
  for (@info) {
    chomp;

    if (/^(URL|Revision|Last Changed Rev):\s*(.+)$/) {
      $svn_info{$path}{$1} = $2;
    }
  }

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $string = &get_command_string ($cmd);
#   $string = &get_command_string (\@cmd);
#
# DESCRIPTION
#   The function returns a string by converting the list in @cmd or the scalar
#   $cmd to a form, where it can be executed as a shell command.
# ------------------------------------------------------------------------------

sub get_command_string {
  my $cmd    = $_[0];
  my $return = '';

  if (ref ($cmd) and ref ($cmd) eq 'ARRAY') {
    # $cmd is a reference to an array

    # Print each argument
    for my $i (0 .. @{ $cmd } - 1) {
      my $arg = $cmd->[$i];

      $arg =~ s/./*/g if $i > 0 and $cmd->[$i - 1] eq '--password';

      if ($arg =~ /[\s'"*?]/) {
        # Argument contains a space, quote it
        if (index ($arg, "'") >= 0) {
          # Argument contains an apostrophe, quote it with double quotes
          $return .= ($i > 0 ? ' ' : '') . '"' . $arg . '"';

        } else {
          # Otherwise, quote argument with apostrophes
          $return .= ($i > 0 ? ' ' : '') . "'" . $arg . "'";
        }

      } else {
        # Argument does not contain a space, just print it
        $return .= ($i > 0 ? ' ' : '') . ($arg eq '' ? "''" : $arg);
      }
    }

  } else {
    # $cmd is a scalar, just print it "as is"
    $return = $cmd;
  }

  return $return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &print_command ($cmd);
#   &print_command (\@cmd);
#
# DESCRIPTION
#   The function prints the list in @cmd or the scalar $cmd, as it would be
#   executed by the shell.
# ------------------------------------------------------------------------------

sub print_command {
  my $cmd = $_[0];

  print '=> ', &get_command_string ($cmd) , "\n";
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @return = &run_command (\@cmd, <OPTIONS>);
#   @return = &run_command ($cmd , <OPTIONS>);
#
# DESCRIPTION
#   This function executes the command in the list @cmd or in the scalar $cmd.
#   The remaining are optional arguments in a hash table. Valid options are
#   listed below. If the command is run using "qx", the function returns the
#   standard output from the command. If the command is run using "system", the
#   function returns true on success. By default, the function dies on failure.
#
# OPTIONS
#   METHOD  => $method - this can be "system", "exec" or "qx". This determines
#                        how the command will be executed. If not set, the
#                        default is to run the command with "system".
#   PRINT   => 1       - if set, print the command before executing it.
#   ERROR   => $flag   - this should only be set if METHOD is set to "system"
#                        or "qx". The $flag can be "die" (default), "warn" or
#                        "ignore". If set to "die", the function dies on error.
#                        If set to "warn", the function issues a warning on
#                        error, and the function returns false. If set to
#                        "ignore", the function returns false on error.
#   RC      => 1       - if set, must be a reference to a scalar, which will be
#                        set to the return code of the command.
#   DEVNULL => 1       - if set, re-direct STDERR to /dev/null before running
#                        the command.
#   TIME    => 1       - if set, print the command with a timestamp before
#                        executing it, and print the time taken when it
#                        completes. This option supersedes the PRINT option.
# ------------------------------------------------------------------------------

sub run_command {
  my $cmd     = shift;
  my %options = @_;
  my $method  = exists $options{METHOD}  ? $options{METHOD}  : 'system';
  my $print   = exists $options{PRINT}   ? $options{PRINT}   : undef;
  my $error   = exists $options{ERROR}   ? $options{ERROR}   : 'die';
  my $rc      = exists $options{RC}      ? $options{RC}      : undef;
  my $devnull = exists $options{DEVNULL} ? $options{DEVNULL} : undef;
  my $time    = exists $options{TIME}    ? $options{TIME}    : undef;
  my @return  = ();

  # Check that the $error flag is set correctly
  $error = 'die' unless $error =~ /^(?:die|warn|ignore)$/i;

  # Print the command before execution, if necessary
  if ($time) {
    print &timestamp_command (&get_command_string ($cmd));

  } elsif ($print) {
    &print_command ($cmd);
  }

  # Re-direct to /dev/null if necessary
  if ($devnull) {
    $devnull = File::Spec->devnull;

    # Save current STDERR
    no warnings;
    open OLDERR, ">&STDERR" or croak 'Cannot dup STDERR (', $!, '), abort';
    use warnings;

    # Redirect STDERR to /dev/null
    open STDERR, '>', $devnull
      or croak 'Cannot redirect STDERR (', $!, '), abort';

    # Make sure the channels are unbuffered
    my $select = select;
    select STDERR; $| = 1;
    select $select;
  }

  if (ref ($cmd) and ref ($cmd) eq 'ARRAY') {
    # $cmd is an array
    my @command = @{ $cmd };

    if ($method eq 'qx') {
      @return = qx(@command);

    } elsif ($method eq 'exec') {
      exec (@command);

    } else {
      system (@command);
      @return = $? ? () : (1);
    }

  } else {
    # $cmd is an scalar
    if ($method eq 'qx') {
      @return = qx($cmd);

    } elsif ($method eq 'exec') {
      exec ($cmd);

    } else {
      system ($cmd);
      @return = $? ? () : (1);
    }
  }

  # Put STDERR back to normal, if redirected previously
  if ($devnull) {
    close STDERR;

    open STDERR, ">&OLDERR" or croak 'Cannot dup STDERR (', $!, '), abort';
  }

  # Print the time taken for command after execution, if necessary
  print &timestamp_command (&get_command_string ($cmd), 'end') if $time;

  if ($?) {
    # The command has failed
    if ($error eq 'die') {
      # Throw fatal error if ERROR is set to "die"
      croak &get_command_string ($cmd), ' failed (', $?, ')';

    } elsif ($error eq 'warn') {
      # Issue warning if ERROR is set to "warn"
      carp  &get_command_string ($cmd), ' failed (', $?, ')';
    }
  }

  # Set the return code if necessary
  $$rc = $? if $rc;

  return @return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &e_report (@message);
#
# DESCRIPTION
#   The function prints @message to STDERR and aborts with a error.
# ------------------------------------------------------------------------------

sub e_report {
  print STDERR @_, "\n" if @_;

  exit 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &w_report (@message);
#
# DESCRIPTION
#   The function prints @message to STDERR and returns.
# ------------------------------------------------------------------------------

sub w_report {
  print STDERR @_, "\n" if @_;

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $date = &svn_date ($time);
#
# DESCRIPTION
#   The function returns a date, formatted as by Subversion. The argument $time
#   is the number of seconds since epoch.
# ------------------------------------------------------------------------------

sub svn_date {
  my $time = shift;

  return strftime ('%Y-%m-%d %H:%M:%S %z (%a, %d %b %Y)', localtime ($time));
}

# ------------------------------------------------------------------------------

1;

__END__
