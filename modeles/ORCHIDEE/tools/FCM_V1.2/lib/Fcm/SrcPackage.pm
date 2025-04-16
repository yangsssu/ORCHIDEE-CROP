#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::SrcPackage
#
# DESCRIPTION
#   This is a class to process a source directory package. It uses the
#   supplied inheritance hierarchy to obtain a list of source files of this
#   package.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::SrcPackage;

# Standard pragma
use strict;
use warnings;

# Standard modules
use Carp;
use File::Spec::Functions;
use File::Basename;
use File::Path;

# FCM component modules
use Fcm::Util;
use Fcm::SrcFile;

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $package = Fcm::SrcPackage->new (
#     CONFIG     => $config,
#     NAME       => $name,
#     CURRENT    => $current,
#     REQUIREPP  => $requirepp,
#     NEWPP      => $newpp,
#     SEARCHPATH => \@path,
#   );
#
# DESCRIPTION
#   This method constructs a new instance of the Fcm::SrcPackage class.
#
# ARGUMENTS
#   CONFIG     - reference to a Fcm::Config instance
#   NAME       - name of the source directory package
#   CURRENT    - package declared in current build?
#   REQUIREPP  - require pre-processing?
#   NEWPP      - pre-process option has changed?
#   SEARCHPATH - search path of files in the source package
# ------------------------------------------------------------------------------

sub new {
  my $this  = shift;
  my %args  = @_;
  my $class = ref $this || $this;

  my $self  = {
    CONFIG     => exists $args{CONFIG}     ? $args{CONFIG}     : &main::cfg,
    NAME       => exists $args{NAME}       ? $args{NAME}       : undef,
    CURRENT    => exists $args{CURRENT}    ? $args{CURRENT}    : undef,
    REQUIREPP  => exists $args{REQUIREPP}  ? $args{REQUIREPP}  : undef,
    NEWPP      => exists $args{NEWPP}      ? $args{NEWPP}      : undef,
    SEARCHPATH => exists $args{SEARCHPATH} ? $args{SEARCHPATH} : [],

    # Reference to Fcm::CfgFile, source package configuration file
    CFG       => undef,

    # References to Fcm::SrcFile, list of source files
    SRCFILE   => [],
  };

  bless $self, $class;
  return $self;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config = $package->config;
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
#   $name = $package->name;
#   $package->name ($name);
#
# DESCRIPTION
#   This method returns the name of this package. If an argument is specified,
#   the name is set to the value of the argument.
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
#   $flag = $package->current;
#   $package->current ($flag);
#
# DESCRIPTION
#   This method returns the "current" flag of the source package. If an
#   argument is specified, the flag is set to the value of the argument.
# ------------------------------------------------------------------------------

sub current {
  my $self = shift;

  if (@_) {
    $self->{CURRENT} = shift;
  }

  return $self->{CURRENT};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $flag = $package->requirepp;
#   $package->requirepp ($flag);
#
# DESCRIPTION
#   This method returns the "require PP" flag of the source package. If an
#   argument is specified, the flag is set to the value of the argument.
# ------------------------------------------------------------------------------

sub requirepp {
  my $self = shift;

  if (@_) {
    $self->{REQUIREPP} = shift;
  }

  return $self->{REQUIREPP};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $flag = $package->newpp;
#   $package->newpp ($flag);
#
# DESCRIPTION
#   This method returns the flag to denote whether pre-processor option for
#   this source package has changed. If an argument is specified, the flag is
#   set to the value of the argument.
# ------------------------------------------------------------------------------

sub newpp {
  my $self = shift;

  if (@_) {
    $self->{NEWPP} = shift;
  }

  return $self->{NEWPP};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $cfgfile = $package->cfg;
#   $package->cfg ($cfgfile);
#
# DESCRIPTION
#   This method returns a reference to a Fcm::CfgFile instance for the source
#   package configuration file. If an argument is specified, the reference is
#   set to the value of the argument.
# ------------------------------------------------------------------------------

sub cfg {
  my $self = shift;

  if (@_) {
    $self->{CFG} = $_[0];
  }

  return $self->{CFG};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @path = $package->searchpath;
#   $package->searchpath (@path);
#
# DESCRIPTION
#   This method returns the source file search path associated with this
#   source package in the current build. If arguments are specified, the
#   search path is replaced by the array in the argument list.
# ------------------------------------------------------------------------------

sub searchpath {
  my $self = shift;

  @{ $self->{SEARCHPATH} } = @_ if @_; 

  return @{ $self->{SEARCHPATH} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @path = $package->ppsearchpath;
#
# DESCRIPTION
#   This method returns the pre-processed source file search path associated
#   with this source package in the current build.
# ------------------------------------------------------------------------------

sub ppsearchpath {
  my $self = shift;

  my @path = ();
  my @name = split /__/, $self->name;

  for my $ppsrcdir (@{ $self->config->setting (qw/PATH PPSRC/) }) {
    push @path, catfile ($ppsrcdir, @name);
  }

  return @path;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $base = $package->flagsbase ($flag);
#
# DESCRIPTION
#   Returns the base name of a flags-file, determined by $flag.
# ------------------------------------------------------------------------------

sub flagsbase {
  my ($self, $flag) = @_;

  return join ('__', ($flag, $self->name)) .
         $self->config->setting (qw/OUTFILE_EXT FLAGS/);
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   @srcfile = $package->srcfile;
#
# DESCRIPTION
#   This method returns a list of references to Fcm::SrcFile instances
#   associated with this package.
# ------------------------------------------------------------------------------

sub srcfile {
  my $self = shift;

  return @{ $self->{SRCFILE} };
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $package->update_file_info ();
#
# DESCRIPTION
#   This method updates the source file information of this package. Please
#   note that information is only updated if the cache file for this package
#   does not exist. For a package declared in the current build, the
#   information is also updated if the cache file is out of date.
# ------------------------------------------------------------------------------

sub update_file_info {
  my $self      = shift;

  # Check if the cache file exists and up to date
  my @cachepath = @{ $self->config->setting (qw/PATH CACHE/) };
  my $cachefile = find_file_in_path ($self->_cache_basename, \@cachepath);

  my $uptodate  = $cachefile ? 1 : 0;
  if ($uptodate and $self->{CURRENT}) {
    # Is cache file up to date compared with directory?
    $uptodate = (stat $cachefile) [9] > (stat $self->{SEARCHPATH}[0]) [9];

    # Is cache file up to date compared with each file?
    if ($uptodate) {
      my $dir = $self->{SEARCHPATH}[0];

      if (opendir DIR, $dir) {
        my @files = map {catfile $dir, $_} grep {!/^\.\.?/} readdir 'DIR';
        closedir DIR;
        $uptodate = (grep {(stat $cachefile) [9] > (stat) [9]} @files) ? 1 : 0;
      }
    }
  }

  # Read package source file information if it appears to be up to date
  $uptodate = $self->_read_file_list_cache ($cachefile) if ($uptodate);

  # Update package source file information if necessary
  if (not $uptodate) {
    # Get list of files by searching through the search path
    my @files = ();
    for my $dir (@{ $self->{SEARCHPATH} }) {
      opendir DIR, $dir;
      while (my $base = readdir 'DIR') {
        next if $base =~ /^\./;

        my $file = catfile $dir, $base;
        next if -d $file;

        push @files, $file unless grep {basename ($_) eq $base} @files;
      }
      closedir DIR;
    }

    # Declare new instances of source file objects
    my @srcfile = ();
    for my $file (@files) {
      if (basename ($file) eq $self->config->setting (qw/CFG_NAME SRCPACKAGE/)) {
        $self->{CFG} = Fcm::CfgFile->new (CONFIG => $self->config, SRC => $file);

      } else {
        my $srcfile = Fcm::SrcFile->new (
          CONFIG     => $self->config,
          SRC        => $file,
          SRCPACKAGE => $self,
        );

        # Determine source file types
        $srcfile->determine_type;

        # Record files of known types
        push @srcfile, $srcfile;
      }
    }

    # Set each SRCFILE to reference the source file instances
    $self->{SRCFILE} = \@srcfile;

    # Decipher configuration file if necessary
    $self->_decipher_cfg if $self->cfg;

    # Write to a new cache file
    $self->_update_file_list_cache ();

    # Source package info updated. Make sure the "current" flag is set to true
    $self->current (1);
  }

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $up_to_date = $self->_read_file_list_cache ($file);
#
# DESCRIPTION
#   This internal method reads the cache $file of this package and assigns the
#   information to the SRCFILE list. It returns true if the cache appears to
#   be up to date.
# ------------------------------------------------------------------------------

sub _read_file_list_cache {
  my $self = shift;
  my $file = shift;

  my $cfg = Fcm::CfgFile->new (CONFIG => $self->config, SRC => $file);

  # Read from config file
  $cfg->read_cfg;
  my @lines = $cfg->lines;

  my %filetype = ();
  my $uptodate = 1;
  for my $line (@lines) {
    next unless $line->{LABEL};

    # On package declared in the current build, check that file is not deleted
    if (not -f $line->{LABEL}) {
      $uptodate = 0;
      last;
    }

    $filetype{$line->{LABEL}} = $line->{VALUE};
  }

  # Assign to SRCFILE list if cache file is up to date
  if ($uptodate) {
    my @srcfiles = ();

    for my $file (sort keys %filetype) {
      if ($filetype{$file} eq 'SRCPACKAGECFG') {
        $self->{CFG} = Fcm::CfgFile->new (CONFIG => $self->config, SRC => $file);

      } else {
        my $srcfile = Fcm::SrcFile->new (
          CONFIG     => $self->config,
          SRC        => $file,
          TYPE       => $filetype{$file},
          SRCPACKAGE => $self,
        );

        push @srcfiles, $srcfile;
      }
    }

    $self->{SRCFILE} = [@srcfiles];

    $self->_decipher_cfg if $self->cfg;
  }

  return $uptodate;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_update_file_list_cache (\@cachepath);
#
# DESCRIPTION
#   This internal method updates the cache file of this package by writing
#   current SRCFILE information to it. The argument @cachepath must be the
#   search path of the build cache directory.
# ------------------------------------------------------------------------------

sub _update_file_list_cache {
  my $self      = shift;
  my @cachepath = @{ $self->config->setting (qw/PATH CACHE/) };

  my $cfg = Fcm::CfgFile->new (CONFIG => $self->config);

  if ($self->{CFG}) {
    $cfg->add_line (LABEL => $self->cfg->src, VALUE => 'SRCPACKAGECFG')
  }

  for my $file (@{ $self->{SRCFILE} }) {
    $cfg->add_line (LABEL => $file->src, VALUE => $file->type);
  }

  my $cachefile = catfile $cachepath[0], $self->_cache_basename;
  $cfg->print_cfg ($cachefile);

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $basename = $self->_cache_basename ($type);
#
# DESCRIPTION
#   This internal method returns the basename of a cache file for this
#   package. If no argument is specified, it returns the package file list
#   cache name. Otherwise, it returns the package file dependency cache name.
# ------------------------------------------------------------------------------

sub _cache_basename {
  my $self = shift;
  my $type = $_[0] ? $_[0] : 'PCKFILE';

  return $self->{NAME} . $self->config->setting ('CACHE', $type);
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $self->_decipher_cfg ();
#
# DESCRIPTION
#   This internal method deciphers the CFG file associated with this source
#   package.
# ------------------------------------------------------------------------------

sub _decipher_cfg {
  my $self = shift;

  $self->cfg->read_cfg;
  my @lines = $self->cfg->lines;

  my %cfg_label = %{ $self->config->setting ('CFG_LABEL') };

  LINE: for my $line (@lines) {
    my $label = $line->{LABEL};
    my $value = $line->{VALUE};

    next unless $label;
    next if uc $label eq $cfg_label{CFGFILE}{TYPE};
    next if uc $label eq $cfg_label{CFGFILE}{VERSION};

    my ($prefix, $name) = split /::/, $label;

    # Get name of file from the package cfg
    my $srcfile;
    if ($name) {
      ($srcfile) = grep {$_->base eq $name} @{ $self->{SRCFILE} };

      # Create new instance of Fcm::SrcFile if not already in package
      if (not $srcfile) {
        my $src    = find_file_in_path ($name, $self->{SEARCHPATH});
        my $target = $name unless $src;

        $srcfile   = Fcm::SrcFile->new (
          CONFIG     => $self->config,
          SRCPACKAGE => $self,
          SRC        => $src ? $src : $name,
          TARGET     => $target,
          PCKCFG     => 1,
        );
        push @{ $self->{SRCFILE} }, $srcfile;

      } else {
        $srcfile->pckcfg (1);
      }

    } else {
      w_report 'Warning: ', $line->{SRC}, ': LINE ', $line->{NUMBER},
               ': label "', $label, '" not recognised.';
      next LINE;
    }

    $prefix = uc $prefix;
    if ($prefix eq $cfg_label{TYPE}) {
      # Type label of source file
      $srcfile->type (uc $value);
      $srcfile->scan (0) if $srcfile->is_type (qw/BINARY LIB/);
      next LINE;

    } elsif ($prefix eq $cfg_label{SCAN}) {
      # Scan original file for dependency?
      $srcfile->scan ($value);
      next LINE;

    } elsif ($prefix eq $cfg_label{TARGET}) {
      # Name of build target for this source file
      $srcfile->exebase ($value);
      next LINE;

    } elsif ($prefix eq $cfg_label{INTNAME}) {
      # Program unit name of this source file
      $srcfile->progname ($value);
      next LINE;

    } elsif ($prefix eq $cfg_label{DEP}) {
      # Dependency of this source file
      my ($type, $target) = split /::/, $value;
      $srcfile->add_dep ($target, uc $type);
      next LINE;

    } else {
      w_report 'Warning: ', $line->{SRC}, ': LINE ', $line->{NUMBER},
               ': label "', $label, '" not recognised.';
      next LINE;
    }
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $package->scan_dependency ();
#   $package->scan_dependency (HEADER_ONLY => 1);
#
# DESCRIPTION
#   This method scans the dependency in each source file in this source
#   package and updates the package dependency cache. If HEADER_ONLY is
#   specified, it performs dependency scan for pre-processor headers only if
#   this source package requires pre-processing.
# ------------------------------------------------------------------------------

sub scan_dependency {
  my $self = shift;
  my %args = @_;

  # Search for include header dependencies only
  my $header_only = exists $args{HEADER_ONLY} ? $args{HEADER_ONLY} : 0;

  # Get list of source files
  # If header dependencies only, only consider FPP, C and CPP files
  my @srcfiles = $header_only
                 ? grep {$_->is_type_or (qw/FPP C CPP/)} $self->srcfile
                 : grep {$_->type} $self->srcfile;
  return unless @srcfiles;

  # Location of the cache
  my @cachepath = @{ $self->config->setting (qw/PATH CACHE/) };
  my $cachebase = $header_only
                  ? $self->_cache_basename ('PCKPPDEPEND')
                  : $self->_cache_basename ('PCKDEPEND');
  my $cachefile = find_file_in_path ($cachebase, \@cachepath);

  # Obtain old dependency information from cache file if it exists
  my %dep     = ();
  my %intname = ();

  if ($cachefile) {
    # Read the cache
    my $cfg = Fcm::CfgFile->new (CONFIG => $self->config, SRC => $cachefile);
    $cfg->read_cfg;
    my @lines = $cfg->lines;

    # Get list of source file base names
    my %srcfilebase;
    for (@srcfiles) {
      my $base = $_->ppsrc ? $_->ppbase : $_->base;
      $srcfilebase{$base} = 1;
    }

    for my $line (@lines) {
      next unless $line->{LABEL};

      # Label is either INTNAME or a dependency type name
      # For INTNAME, value is the program unit name
      # Otherwise, value is file::dependency
      my $type = $line->{LABEL};
      (my $file, my $depend) = split /::/, $line->{VALUE};

      # Make sure $file exists in the list of source file base names
      next unless exists $srcfilebase{$file};

      if ($type eq 'INTNAME') {
        $intname{$file} = $depend;

      } else {
        $dep{$file}{$depend} = $type;
      }
    }
  }

  # If a source file is newer than the cache file, re-scan dependency for that
  # source file.
  my $uptodate        = $cachefile ? 1 : 0;
  my $cachefile_mtime = $cachefile ? (stat $cachefile) [9] : undef;
  my $count           = 0;

  for my $srcfile (@srcfiles) {
    # Check modified time of source file
    my $srcfile_mtime = $srcfile->mtime;

    # If a package config file exists and it affects the source file,
    # compare its timestamp with that of the source file
    if ($srcfile->pckcfg) {
      $srcfile_mtime = $self->cfg->mtime if not defined $srcfile_mtime;
      $srcfile_mtime = ($self->cfg->mtime > $srcfile_mtime) ? $self->cfg->mtime
                                                            : $srcfile_mtime;
    }

    # For files requiring PP, must re-scan if PP option has changed
    my $rescan = ($self->newpp and $srcfile->is_type_or (qw/FPP C/)) ? 1 : 0;

    if ($cachefile_mtime and $cachefile_mtime > $srcfile_mtime and ! $rescan) {
      # No need to re-scan dependency, read dependency from cache
      my $base = ($srcfile->ppsrc ? $srcfile->ppbase : $srcfile->base);

      $srcfile->progname ($intname{$base}) if $intname{$base};
      $srcfile->dep ($dep{$base})          if $dep{$base};

    } else {
      # Rescan dependency
      $srcfile->progname (undef);
      my $rc = $srcfile->scan_dependency (HEADER_ONLY => $header_only);
      my %dp = $srcfile->dep;

      # Get list of dependencies for updating the cache
      my $base = ($srcfile->ppsrc ? $srcfile->ppbase : $srcfile->base);

      $intname{$base} = $srcfile->progname;
      $dep    {$base} = \%dp;

      $uptodate = 0;
      $count++ if $rc;
    }
  }

  # Output diagnostic, if necessary
  if ($self->config->verbose > 1 and $count) {
    my $out =  $self->name . ': scanned ' . $count . ' file(s) for';
    $out   .= ' header' if $header_only;
    $out   .= ' dependency' . "\n";
    print $out;
  }

  # Check whether package config file is newer than the dependency cache
  if ($uptodate and $self->cfg) {
    $uptodate = $cachefile_mtime > $self->cfg->mtime ? 1 : 0;
  }

  if (not $uptodate) {
    # Update dependency cache file
    my $cfg = Fcm::CfgFile->new (CONFIG => $self->config);

    # Program unit name of source files
    for my $file (keys %intname) {
      next unless $intname{$file};

      $cfg->add_line (
        LABEL => 'INTNAME',
        VALUE => $file . '::' . $intname{$file},
      );
    }

    # Dependencies of source files
    for my $file (keys %dep) {
      for my $depend (keys %{ $dep{$file} }) {
        $cfg->add_line (
          LABEL => $dep{$file}{$depend},
          VALUE => $file . '::' . $depend,
        );
      }
    }

    # Create an empty config file if no dependency in this source package
    $cfg->add_line unless $cfg->lines;

    # Write to config file
    my $outfile = catfile $cachepath[0], $cachebase;
    $cfg->print_cfg ($outfile);
  }

  return not $uptodate;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $rc = $package->makerule_uptodate ();
#
# DESCRIPTION
#   This method returns true if the make rule file for this source package
#   is up to date.
# ------------------------------------------------------------------------------

sub makerule_uptodate {
  my $self = shift;

  my $return = 0;

  if (not $self->newpp) {
    # Check whether a Make rule file already exists
    my $mkbase = $self->name . $self->config->setting (qw/OUTFILE_EXT MK/);
    my $mkfile = find_file_in_path (
      $mkbase,
      $self->config->setting (qw/PATH BLD/),
    );

    # Check location of source package file type cache
    my $pckfile = find_file_in_path (
      $self->_cache_basename ('PCKFILE'),
      $self->config->setting (qw/PATH CACHE/),
    );

    # Check location of source package dependency cache
    my $pckdepend = find_file_in_path (
      $self->_cache_basename ('PCKDEPEND'),
      $self->config->setting (qw/PATH CACHE/),
    );

    # If make rule file exists, determine whether it is out of date
    if ($pckdepend) {
      if ($mkfile) {
        my $pckfile_mt   = (stat $pckfile)  [9];
        my $pckdepend_mt = (stat $pckdepend)[9];
        my $mkfile_mt    = (stat $mkfile)   [9];

        $return = 1 if $mkfile_mt >= $pckdepend_mt and $mkfile_mt >= $pckfile_mt;
      }

    } else {
      $return = 1; # No cache file, no need to have a make rule
    }
  }

  return $return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $package->write_makerule ();
#
# DESCRIPTION
#   This method writes to the I<Make> rule file of the current source package.
# ------------------------------------------------------------------------------

sub write_makerule {
  my $self = shift;

  # Package Make rule header
  my $mk = '# Automatic Make rule for ' . $self->name . "\n\n";

  # Set up variable for directory name
  # if package name contains only word characters
  my @searchpath   = $self->searchpath;
  my @ppsearchpath = $self->ppsearchpath;

  if ($self->name =~ /^\w+$/) {
    # Package search path
    my %path = (SRCDIR => \@searchpath, PPSRCDIR => \@ppsearchpath);

    for my $key (keys %path) {
      my $count = 0;
      my @dirs  = @{ $path{$key} };

      for my $i (0 .. $#dirs) {
        next unless -d $dirs[$i];
        $mk .= $key . $i . '__' . $self->name . ' = ' . $dirs[$i] . "\n";
        $count++;
      }

      $mk .= "\n" if $count;
    }
  }

  my $mk_out;

  # Make rules for copying data files, if necessary
  {
    # Get a list of files with no associated type
    my @files = grep {not $_->type} @{ $self->{SRCFILE} };

    if (@files) {
      my $target = $self->name . $self->config->setting (qw/OUTFILE_EXT ETC/);
      $mk_out .= $target . ' :';

      # Depends on all un-typed source files
      my $nl = " \\\n" . ' ' x 10;
      for my $file (@files) {
        my $dir = $file->dir;

        # Use variable for directory name
        # if package name contains only word characters
        if ($self->name =~ /^\w+$/) {
          for my $i (0 .. $#searchpath) {
            if ($dir eq $searchpath[$i]) {
              $dir = '$(SRCDIR' . $i . '__' . $self->name . ')';
              last;
            }
          }
        }

        $mk_out .= $nl . catfile ($dir, $file->base);
      }

      # Depends on dummy copy file, so there will be no dependency inheritance
      $mk_out .= $nl . $self->config->setting (qw/MISC CPDUMMY/);

      # Actions for target
      $mk_out .= "\n";
      $mk_out .= "\t" . 'cp $^ $(FCM_ETCDIR)' . "\n";
      $mk_out .= "\t" . 'touch ' . catfile ('$(FCM_DONEDIR)', '$@') . "\n";

      $mk_out .= "\n";
    }
  }

  # Make rules for source files
  my @srcfiles = grep {$_->type} @{ $self->{SRCFILE} };
  for my $srcfile (@srcfiles) {
    $mk_out .= $srcfile->write_makerules;
  }

  # Write make rule file only if necessary
  if ($mk_out) {
    $mk .= $mk_out;

    # Write to output file
    my $mkbase = $self->name . $self->config->setting (qw/OUTFILE_EXT MK/);
    my $blddir = ${ $self->config->setting (qw/PATH BLD/) }[0];
    my $mkfile = catfile $blddir, $mkbase;

    if (not -d $blddir) {
      print 'Make directory: ', $blddir, "\n" if $self->config->verbose > 1;
      mkpath $blddir or croak $blddir, ': cannot create directory, abort';
    }

    open OUT, '>', $mkfile
      or croak 'Cannot open "', $mkfile, '" (', $!, '), abort';
    print OUT $mk;
    close OUT or croak 'Cannot close "', $mkfile, '" (', $!, '), abort';

    print 'Generated: ', $mkfile, "\n" if $self->config->verbose > 1;

    return 1;

  } else {
    return 0;
  }
}

# ------------------------------------------------------------------------------

1;

__END__
