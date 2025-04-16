#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::Config
#
# DESCRIPTION
#   This is a class for reading and processing central and user configuration
#   settings for FCM.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::Config;

# Standard pragma
use warnings;
use strict;

# Standard modules
use File::Basename;
use File::Spec::Functions;
use FindBin;
use POSIX qw/setlocale LC_ALL/;

# FCM component modules
use Fcm::CfgFile;

# Other declarations:
sub _get_hash_value;

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config = Fcm::Config->new (VERBOSE => $verbose);
#
# DESCRIPTION
#   This method constructs a new instance of the Fcm::Config class.
#
# ARGUMENTS
#   VERBOSE - Set the verbose level of diagnostic output
# ------------------------------------------------------------------------------

sub new {
  my $this  = shift;
  my %args  = @_;
  my $class = ref $this || $this;

  # Ensure that all subsequent Subversion output is in UK English
  if (setlocale (LC_ALL, 'en_GB')) {
    $ENV{LANG} = 'en_GB';
  }

  # Location of the central/user configuration file
  my $cntl_config = catfile (dirname ($FindBin::Bin), 'etc', 'fcm.cfg');
  $cntl_config    = catfile $FindBin::Bin, 'fcm.cfg' unless -r $cntl_config;
  my $user_config = exists $ENV{HOME} ? catfile $ENV{HOME}, '.fcm' : '';

  # Verbose mode
  my $verbose     = exists $ENV{FCM_VERBOSE} ? $ENV{FCM_VERBOSE} : 1;

  my $self = {
    CNTL_CONFIG => -r $cntl_config ? $cntl_config : '',
    USER_CONFIG => -r $user_config ? $user_config : '',
    VERBOSE     => exists $args{VERBOSE} ? $args{VERBOSE} : $verbose,
    VARIABLE    => {},

    # Primary settings
    SETTING => {
      # Release identifier/version
      RELEASE  => '1.2',

      # Location of file with the last changed revision of the FCM trunk
      REV_FILE => catfile (dirname ($FindBin::Bin), 'etc', 'fcm_rev'),

      # Default names of known FCM configuration files
      CFG_NAME => {
        BLD        => 'bld.cfg',      # bld cfg
        EXT        => 'ext.cfg',      # ext cfg
        SRCPACKAGE => '@PACKAGE.cfg', # source package cfg
      },

      # Latest version of known FCM configuration files
      CFG_VERSION => {
        BLD        => '1.0', # bld cfg
        EXT        => '1.0', # ext cfg
        SRCPACKAGE => '1.0', # source package cfg
      },

      # Labels for all types of FCM configuration files
      CFG_LABEL => {
        CFGFILE => {
          TYPE    => 'CFG::TYPE',       # config file type
          VERSION => 'CFG::VERSION',    # version of config file syntax
        },

        # Labels for central/user internal config setting
        SETTING    => 'SET',

        # Labels for ext and bld cfg
        USE        => 'USE',            # use (inherit from) another ext/bld
        SRCDIR     => 'SRC',            # prefix, source directory

        # Labels for bld and pck cfg
        TARGET     => 'TARGET',         # BLD: targets of current build
                                        # PCK: target name of source file

        # Labels for bld cfg
        NAME        => 'NAME',          # build name
        DIR         => 'DIR',           # prefix, build directory
        PP          => 'PP',            # prefix, pre-process?
        LIB         => 'LIB',           # declare name of a library
        SEARCH_SRC  => 'SEARCH_SRC',    # search src/ sub-directory?
        EXE_NAME    => 'EXE_NAME',      # rename a main program target
        TOOL        => 'TOOL',          # prefix, build tool
        INHERIT     => 'INHERIT',       # prefix, inheritance flag
        EXCL_DEP    => 'EXCL_DEP',      # exclude these automatic dependencies
        INFILE_EXT  => 'INFILE_EXT',    # input file name extension and type
        OUTFILE_EXT => 'OUTFILE_EXT',   # output file type and name extension
        EXE_DEP     => 'EXE_DEP',       # extra executable dependencies
        BLOCKDATA   => 'BLOCKDATA',     # BLOCKDATA dependencies

        # Labels for ext cfg
        DEST       => {                 # local extract destinations
          ROOTDIR  => 'DEST::ROOTDIR',  # top directory for this extract
          SRCDIR   => 'DEST::SRCDIR',   # extracted source directory
          CFGDIR   => 'DEST::CFGDIR',   # generated configuration directory
          CACHEDIR => 'DEST::CACHEDIR', # cache directory for fast extract
          BLD_CFG  => 'DEST::BLD_CFG',  # generated bld cfg file
          EXT_CFG  => 'DEST::EXT_CFG',  # generated ext cfg file
        },
        RDEST      => {                 # remote extract destionations
          MACHINE  => 'RDEST::MACHINE', # name of remote machine
          LOGNAME  => 'RDEST::LOGNAME', # user logname on remote machine
          ROOTDIR  => 'RDEST::ROOTDIR', # top directory for this extract
          SRCDIR   => 'RDEST::SRCDIR',  # extracted source directory
          CFGDIR   => 'RDEST::CFGDIR',  # generated configuration directory
          BLD_CFG  => 'RDEST::BLD_CFG', # generated bld cfg file
          EXT_CFG  => 'RDEST::EXT_CFG', # generated ext cfg file
        },
        INC        => 'INC',            # "include" settings in another cfg file
        BDECLARE   => 'BLD',            # declare entries for build system
        OVERRIDE   => 'OVERRIDE',       # set conflict override option
        REPOS      => 'REPOS',          # set repos loc for a project branch
        VERSION    => 'VERSION',        # set version for a project branch
        EXPSRCDIR  => 'EXPSRC',         # prefix, expandable source directory
        MIRROR     => 'MIRROR',         # mirror tool

        # Labels for pck cfg
        TYPE       => 'TYPE',           # type of source file/build task
        SCAN       => 'SCAN',           # scan source file for dependency
        INTNAME    => 'INTNAME',        # internal name of source file
        DEP        => 'DEP',            # source file/build task dependencies
      },

      # Keywords in known FCM configuration files
      CFG_KEYWORD => 'USE,INC,TARGET,EXCL_DEP',

      # Types of "inc" statements expandable CFG files
      CFG_EXP_INC => 'BLD,EXT,FCM',

      # Standard sub-directories for extract/build
      DIR => {
        BIN    => 'bin',    # executable
        BLD    => 'bld',    # build
        CACHE  => '.cache', # cache
        CFG    => 'cfg',    # configuration
        DONE   => 'done',   # "done"
        ETC    => 'etc',    # miscellaneous items
        FLAGS  => 'flags',  # "flags"
        INC    => 'inc',    # include
        LIB    => 'lib',    # library
        OBJ    => 'obj',    # object
        PPSRC  => 'ppsrc',  # pre-processed source
        SRC    => 'src',    # source
        TMP    => 'tmp',    # temporary directory
      },

      # Build commands and options (i.e. tools)
      TOOL => {
        SHELL        => '/usr/bin/ksh',    # Default shell

        CPP          => 'cpp',             # C pre-processor
        CPPFLAGS     => '-C',              # CPP flags
        CPP_INCLUDE  => '-I',              # CPP flag, specify "include" path
        CPP_DEFINE   => '-D',              # CPP flag, define macro
        CPPKEYS      => '',                # CPP keys (definition macro)

        CC           => 'cc',              # C compiler
        CFLAGS       => '',                # CC flags
        CC_COMPILE   => '-c',              # CC flag, compile only
        CC_OUTPUT    => '-o',              # CC flag, specify output file name
        CC_INCLUDE   => '-I',              # CC flag, specify "include" path
        CC_DEFINE    => '-D',              # CC flag, define macro

        FPP          => 'cpp',             # Fortran pre-processor
        FPPFLAGS     => '-P -traditional', # FPP flags
        FPP_INCLUDE  => '-I',              # FPP flag, specify "include" path
        FPP_DEFINE   => '-D',              # FPP flag, define macro
        FPPKEYS      => '',                # FPP keys (definition macro)

        FC           => 'f90',             # Fortran compiler
        FFLAGS       => '',                # FC flags
        FC_COMPILE   => '-c',              # FC flag, compile only
        FC_OUTPUT    => '-o',              # FC flag, specify output file name
        FC_INCLUDE   => '-I',              # FC flag, specify "include" path
        FC_DEFINE    => '-D',              # FC flag, define macro

        LD           => 'ld',              # linker
        LDFLAGS      => '',                # LD flags
        LD_OUTPUT    => '-o',              # LD flag, specify output file name
        LD_LIBSEARCH => '-L',              # LD flag, specify "library" path
        LD_LIBLINK   => '-l',              # LD flag, specify link library

        AR           => 'ar',              # library archiver
        ARFLAGS      => 'rs',              # AR flags

        MAKE         => 'make',            # make command
        MAKEFLAGS    => '',                # make flags
        MAKE_SILENT  => '-s',              # make flag, silent diagnostic
        MAKE_JOB     => '-j',              # make flag, number of jobs

        INTERFACE    => 'file',            # name interface after file/program
        GENINTERFACE => 'ECMWF',           # Fortran 9x interface generator

        MIRROR       => 'rsync',           # extract mirroring tool
        REMOTE_SHELL => 'remsh',           # command to invoke the remote shell
        GRAPHIC_DIFF => 'xxdiff',          # graphical diff tool
      },

      # List of tools that are local to FCM, (will not be exported to a Makefile)
      LOCALTOOL => 'CPP,CPPFLAGS,CPP_INCLUDE,CPP_DEFINE,FPP,FPPFLAGS,' .
                   'FPP_INCLUDE,FPP_DEFINE,GRAPHIC_DIFF,MAKE,MAKEFLAGS,' .
                   'MAKE_SILENT,MAKE_JOB,INTERFACE,GENINTERFACE,MIRROR,' .
                   'REMOTE_SHELL',

      # Supported tools for compilable source
      SRC_TOOL => {
        FORTRAN => {
          COMPILER => 'FC',
          FLAGS    => 'FFLAGS',
          OUTPUT   => 'FC_OUTPUT',
          INCLUDE  => 'FC_INCLUDE',
        },

        FPP     => {
          COMPILER => 'FC',
          FLAGS    => 'FFLAGS',
          PPKEYS   => 'FPPKEYS',
          OUTPUT   => 'FC_OUTPUT',
          INCLUDE  => 'FC_INCLUDE',
          DEFINE   => 'FC_DEFINE',
        },

        C       => {
          COMPILER => 'CC',
          FLAGS    => 'CFLAGS',
          PPKEYS   => 'CPPKEYS',
          OUTPUT   => 'CC_OUTPUT',
          INCLUDE  => 'CC_INCLUDE',
          DEFINE   => 'CC_DEFINE',
        },
      },

      # Cache file names/extensions
      CACHE => {
        EXTCONFIG   => '.config',       # ext cache, commit version info
        PCKFILE     => '.pck_file',     # bld cache, source package list
        PCKPPDEPEND => '.pck_ppdepend', # bld cache, source package PP dependency
        PCKDEPEND   => '.pck_depend',   # bld cache, source package dependency
        BLDTOOL     => '.bld_tool',     # bld cache, build tool list
        PPOPTION    => '.bld_pp',       # bld cache, PP option
        EXE_DEP     => '.exe_dep',      # bld cache, executable extra dependency
      },

      # Input file name extension and type
      # (may overlap with output and vpath, see below)
      INFILE_EXT => {
        # General extensions
        'f'    => 'FORTRAN::SOURCE',
        'for'  => 'FORTRAN::SOURCE',
        'ftn'  => 'FORTRAN::SOURCE',
        'f77'  => 'FORTRAN::SOURCE',
        'f90'  => 'FORTRAN::FORTRAN9X::SOURCE',
        'f95'  => 'FORTRAN::FORTRAN9X::SOURCE',
        'F'    => 'FPP::SOURCE',
        'FOR'  => 'FPP::SOURCE',
        'FTN'  => 'FPP::SOURCE',
        'F77'  => 'FPP::SOURCE',
        'F90'  => 'FPP::FPP9X::SOURCE',
        'F95'  => 'FPP::FPP9X::SOURCE',
        'c'    => 'C::SOURCE',
        'cpp'  => 'C::C++::SOURCE',
        'h'    => 'CPP::INCLUDE',
        'o'    => 'BINARY::OBJ',
        'obj'  => 'BINARY::OBJ',
        'exe'  => 'BINARY::EXE',
        'a'    => 'BINARY::LIB',
        'sh'   => 'SHELL::SCRIPT',
        'ksh'  => 'SHELL::SCRIPT',
        'bash' => 'SHELL::SCRIPT',
        'csh'  => 'SHELL::SCRIPT',
        'pl'   => 'PERL::SCRIPT',
        'pm'   => 'PERL::SCRIPT',
        'py'   => 'PYTHON::SCRIPT',
        'tcl'  => 'TCL::SCRIPT',
        'pro'  => 'PVWAVE::SCRIPT',

        # Local extensions
        'cfg'       => 'CFGFILE',
        'h90'       => 'CPP::INCLUDE',
        'inc'       => 'FORTRAN::FORTRAN9X::INCLUDE',
        'interface' => 'FORTRAN::FORTRAN9X::INCLUDE::INTERFACE',
      },

      # Input file name pattern and type
      INFILE_PAT => {
        '\w+Scr_\w+'              => 'SHELL::SCRIPT',
        '\w+Comp_\w+'             => 'SHELL::SCRIPT::GENTASK',
        '\w+(?:IF|Interface)_\w+' => 'SHELL::SCRIPT::GENIF',
        '\w+Suite_\w+'            => 'SHELL::SCRIPT::GENSUITE',
        '\w+List_\w+'             => 'SHELL::SCRIPT::GENLIST',
        '\w+Sql_\w+'              => 'SCRIPT::SQL',
      },

      # Input text file pattern and type
      INFILE_TXT => {
        '(?:[ck]|ba)?sh'  => 'SHELL::SCRIPT',
        'perl'            => 'PERL::SCRIPT',
        'python'          => 'PYTHON::SCRIPT',
        'tcl(?:sh)?|wish' => 'TCL::SCRIPT',
      },

      # Ignore input files matching the following names (comma-separated list)
      INFILE_IGNORE => 'fcm_env.ksh',

      # Output file type and extension
      # (may overlap with input (above) and vpath (below))
      OUTFILE_EXT => {
        CFG       => '.cfg',       # FCM configuration file
        DONE      => '.done',      # "done" files for compiled source
        ETC       => '.etc',       # "etc" dummy file
        EXE       => '.exe',       # binary executables
        FLAGS     => '.flags',     # "flags" files, compiler flags config
        IDONE     => '.idone',     # "done" files for included source
        INTERFACE => '.interface', # interface for F90 subroutines/functions
        LIB       => '.a',         # archive object library
        MK        => '.mk',        # dependency files, Makefile fragments
        MOD       => '.mod',       # compiled Fortran module information files
        OBJ       => '.o',         # compiled object files
        PDONE     => '.pdone',     # "done" files for pre-processed files
        TAR       => '.tar',       # TAR archive
      },

      # VPATH, each value must be a comma separate list
      # EMPTY      translates to %
      # IN:<FLAG>  translates to any key in {INFILE_EXT} if the value contains
      #            the word in <FLAG>
      # OUT:<FLAG> translates to {OUTFILE_EXT}{<FLAG>}
      VPATH   => {
        BIN   => 'EMPTY,OUT:EXE,IN:SCRIPT',
        BLD   => 'OUT:MK',
        DONE  => 'OUT:DONE,OUT:IDONE,OUT:ETC',
        FLAGS => 'OUT:FLAGS',
        INC   => 'IN:INCLUDE',
        LIB   => 'OUT:LIB',
        OBJ   => 'OUT:OBJ',
      },

      # Dependency scan types for pre-processing
      PP_DEP_TYPE => {
        FPP => 'H',
        CPP => 'H',
        C   => 'H',
      },

      # Dependency scan types
      DEP_TYPE => {
        FORTRAN => 'USE::INTERFACE::INC::OBJ',
        FPP     => 'USE::INTERFACE::INC::H::OBJ',
        CPP     => 'H::OBJ',
        C       => 'H::OBJ',
        SCRIPT  => 'EXE',
      },

      # Dependency pattern for each type
      DEP_PATTERN => {
        H         => q/^#\s*include\s*['"](\S+)['"]/,
        USE       => q/^\s*use\s+(\w+)/,
        INTERFACE => q/^#?\s*include\s+['"](\S+##OUTFILE_EXT::INTERFACE##)['"]/,
        INC       => q/^\s*include\s+['"](\S+)['"]/,
        OBJ       => q#^\s*(?:/\*|!)\s*depends\s*on\s*:\s*(\S+)#,
        EXE       => q/^\s*(?:#|;)\s*(?:calls|list|if|interface)\s*:\s*(\S+)/,
      },

      # Types that always need to be built
      ALWAYS_BUILD_TYPE => 'PVWAVE,GENLIST,SQL',

      # Types that cannot have duplicated targets
      NO_DUPLICATED_TARGET_TYPE => '',

      # Excluded dependency
      EXCL_DEP => {
        # Fortran intrinsic modules
        'USE::ISO_C_BINDING'            => {'' => 1},
        'USE::IEEE_EXCEPTIONS'          => {'' => 1},
        'USE::IEEE_ARITHMETIC'          => {'' => 1},
        'USE::IEEE_FEATURES'            => {'' => 1},

        # Fortran intrinsic subroutines
        'OBJ::CPU_TIME'                 => {'' => 1},
        'OBJ::GET_COMMAND'              => {'' => 1},
        'OBJ::GET_COMMAND_ARGUMENT'     => {'' => 1},
        'OBJ::GET_ENVIRONMENT_VARIABLE' => {'' => 1},
        'OBJ::MOVE_ALLOC'               => {'' => 1},
        'OBJ::MVBITS'                   => {'' => 1},
        'OBJ::RANDOM_NUMBER'            => {'' => 1},
        'OBJ::RANDOM_SEED'              => {'' => 1},
        'OBJ::SYSTEM_CLOCK'             => {'' => 1},

        # Dummy statements
        'OBJ::NONE'                     => {'' => 1},
        'EXE::NONE'                     => {'' => 1},
      },

      # Extra executable dependencies
      EXE_DEP => {},

      # Fortran BLOCKDATA dependencies
      BLOCKDATA => {},

      # Rename main program targets
      EXE_NAME => {},

      # Build sub-directories that can be archived by "tar"
      TAR_DIRS => 'BLD,CACHE,DONE,FLAGS,INC,PPSRC,OBJ',

      # Misc
      MISC => {
        CPDUMMY       => '$(FCM_DONEDIR)/FCM_CP.dummy',
                                         # build system "copy" dummy target
        DIR_SEPARATOR => '/',            # repository directory separator
        EXPURL_PREFIX => 'fcm:',         # expandable URL keyword prefix
        LOCK_BLD      => 'fcm.bld.lock', # build lock file
        LOCK_EXT      => 'fcm.ext.lock', # extract lock file
        MAKEFILE      => 'Makefile',     # name of Makefile
        RUN_ENV_SH    => 'fcm_env.ksh',  # bld runtime environment shell script
        WEB_BROWSER   => 'firefox',      # web browser
      },

      # URL, revision, and Trac URL keywords
      URL      => {},
      REVISION => {},
      TRAC     => {},
    },
  };

  # Backward compatibility: the REPOS setting is equivalent to the URL setting
  $self->{SETTING}{REPOS} = $self->{SETTING}{URL};

  bless $self, $class;
  return $self;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $file = $config->central_config ();
#   $config->central_config ($file);
#
# DESCRIPTION
#   This method returns the path name of the central configuration file. If an
#   argument $file is specified, the path name of the central configuration
#   file is set to its value.
# ------------------------------------------------------------------------------

sub central_config {
  my $self = shift;

  if (@_) {
    $self->{CNTL_CONFIG} = $_[0];
  }

  return $self->{CNTL_CONFIG};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $file = $config->user_config ();
#   $config->user_config ($file);
#
# DESCRIPTION
#   This method returns the path name of the user configuration file. If an
#   argument $file is specified, the path name of the user configuration file
#   is set to its value.
# ------------------------------------------------------------------------------

sub user_config {
  my $self = shift;

  if (@_) {
    $self->{USER_CONFIG} = $_[0];
  }

  return $self->{USER_CONFIG};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $mode = $config->verbose ();
#   $config->verbose ($mode);
#
# DESCRIPTION
#   This method returns the diagnostic verbose level. If an argument $mode is
#   specified, the diagnostic verbose level is set to its value.
# ------------------------------------------------------------------------------

sub verbose {
  my $self = shift;

  if (@_) {
    $self->{VERBOSE} = $_[0];
  }

  return $self->{VERBOSE};
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $setting = $config->setting (arg, [...]);
#
# DESCRIPTION
#   This method returns an item under the SETTING hash table. The depth within
#   the hash table is given by the list of arguments, which should match with
#   the keys in the multi-dimension SETTING hash table.
# ------------------------------------------------------------------------------

sub setting {
  my $self = shift;

  if (@_) {
    my $label   = shift;
    my $setting = $self->{SETTING};
    return _get_hash_value ($setting->{$label}, @_) if exists $setting->{$label};
  }

  return undef;

}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config->assign_setting (
#     LABELS => \@labels, # setting labels
#     VALUE  => $value,   # setting value
#   );
#
# DESCRIPTION
#   This method assigns a VALUE to a SETTING specified by the names in LABEL.
# ------------------------------------------------------------------------------

sub assign_setting {
  my $self = shift;
  my %args = @_;

  my @labels = exists $args{LABELS} ? @{ $args{LABELS} } : ();
  my $value  = exists $args{VALUE}  ? $args{VALUE}       : undef;

  my $setting = $self->{SETTING};
  while (defined (my $label = shift @labels)) {
    if (exists $setting->{$label}) {
      if (ref $setting->{$label}) {
        $setting = $setting->{$label};

      } else {
        $setting->{$label} = $value;
        last;
      }

    } else {
      if (@labels) {
        $setting->{$label} = {};
        $setting           = $setting->{$label};

      } else {
        $setting->{$label} = $value;
      }
    }
  }

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $variable = $config->variable ([arg]);
#
# DESCRIPTION
#   If arg is set, this method returns the value of a variable named arg. If
#   arg is not set, this method returns the VARIABLE hash.
# ------------------------------------------------------------------------------

sub variable {
  my $self     = shift;

  my $variable = $self->{VARIABLE};

  if (@_) {
    my $label   = shift;
    return exists $variable->{$label} ? $variable->{$label} : undef;

  } else {
    return %{ $variable };
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config->assign_variable (
#     LABEL => $label, # variable label
#     VALUE => $value, # variable value
#   );
#
# DESCRIPTION
#   This method assigns a VALUE to a VARIABLE named by LABEL.
# ------------------------------------------------------------------------------

sub assign_variable {
  my $self = shift;
  my %args = @_;

  my $label = exists $args{LABEL} ? $args{LABEL} : undef;
  my $value = exists $args{VALUE} ? $args{VALUE} : undef;

  if ($label) {
    $self->{VARIABLE}{$label} = $value;
  }

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config->get_config ();
#
# DESCRIPTION
#   This method reads the configuration settings from the central and the user
#   configuration files.
# ------------------------------------------------------------------------------

sub get_config {
  my $self = shift;

  $self->_read_config_file ($self->{CNTL_CONFIG}) if $self->{CNTL_CONFIG};  
  $self->_read_config_file ($self->{USER_CONFIG}) if $self->{USER_CONFIG};

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $config->_read_config_file ();
#
# DESCRIPTION
#   This internal method reads a configuration file and assign values to the
#   attributes of the current instance.
# ------------------------------------------------------------------------------

sub _read_config_file {
  my $self        = shift;
  my $config_file = $_[0];

  return undef unless -r $config_file;

  my $cfgfile = Fcm::CfgFile->new (SRC => $config_file, TYPE => 'FCM');
  $cfgfile->read_cfg ();
  my @lines = $cfgfile->lines ();

  LINE: for my $line (@lines) {
    my $label = $line->{LABEL};
    my $value = $line->{VALUE};

    next unless $label;

    # "Environment variables" start with $
    if ($label =~ s/^\$([A-Za-z_]\w*)$/$1/) {
      $ENV{$label} = $value;
      next LINE;
    }

    # "Settings variables" start with "set::"
    my @tags = map {uc $_} split (/::/, $label);
    if ($tags[0] eq uc $self->{SETTING}{CFG_LABEL}{SETTING}) {
      shift @tags;
      $self->assign_setting (LABELS => \@tags, VALUE => $value);
      next LINE;
    }

    # Not a standard setting variable, put in internal variable list
    $label =~ s/^\%//;
    $self->assign_variable (LABEL => $label, VALUE => $value);
  }

  1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $ref = _get_hash_value (arg1, arg2, ...);
#
# DESCRIPTION
#   This internal method recursively gets a value from a multi-dimensional
#   hash.
# ------------------------------------------------------------------------------

sub _get_hash_value {
  my $value = shift;

  while (defined (my $arg = shift)) {
    if (exists $value->{$arg}) {
      $value = $value->{$arg};

    } else {
      return undef;
    }
  }

  return $value;
}

# ------------------------------------------------------------------------------

1;

__END__
