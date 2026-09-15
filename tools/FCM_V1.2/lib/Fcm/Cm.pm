#!/usr/bin/perl
# ------------------------------------------------------------------------------
# NAME
#   Fcm::Cm
#
# DESCRIPTION
#   This module contains the FCM code management functionalities and wrappers
#   to Subversion commands.
#
# COPYRIGHT
#   (C) Crown copyright Met Office. All rights reserved.
#   For further details please refer to the file COPYRIGHT.txt
#   which you should have received as part of this distribution.
# ------------------------------------------------------------------------------

package Fcm::Cm;

# Standard pragma
use warnings;
use strict;

# Standard modules
our (@ISA, @EXPORT, @EXPORT_OK);
use Getopt::Long;
use File::Basename;
use File::Path;
use File::Spec;
use File::Temp qw/tempfile/;
use Cwd;

# FCM component modules
use Fcm::CmBranch;
use Fcm::CmUrl;
use Fcm::Util;

sub cm_command;

require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(
  cm_command
);

# Function declaration:
sub cm_add;
sub cm_branch;
sub cm_commit;
sub cm_conflicts;
sub cm_delete;
sub cm_diff;
sub cm_merge;
sub cm_mkpatch;
sub cm_svn;
sub cm_switch;
sub _construct_branch_url;

# ------------------------------------------------------------------------------

my @subcommand_url = qw/
  blame     praise annotate ann
  branch    br
  cat
  checkout  co
  copy      cp
  delete    del    remove   rm
  diff      di
  export
  import
  info
  list      ls
  lock
  log
  merge
  mkdir
  mkpatch
  move      mv     rename   ren
  propdel   pdel   pd
  propedit  pedit  pe
  propget   pget   pg
  proplist  plist  pl
  propset   pset   ps
  switch    sw
  unlock
/; # List of subcommands that accept URL inputs

my @subcommand_rev = qw/
  blame     praise annotate ann
  branch    br
  cat
  checkout  co
  copy      cp
  diff      di
  export
  info
  list      ls
  log
  merge
  mkpatch
  move      mv     rename   ren
  propdel   pdel   pd
  propedit  pedit  pe
  propget   pget   pg
  proplist  plist  pl
  propset   pset   ps
  switch    sw
  update    up
/; # List of subcommands that accept revision inputs

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &cm_command ($function);
#
# DESCRIPTION
#   This is the generic FCM code management wrapper. It calls the correct FCM
#   code management function or a wrapper to a Subversion command based on the
#   value of the argument $function.
# ------------------------------------------------------------------------------

sub cm_command {

  my ($function) = shift @_;

  # Expand URL keywords if necessary
  if (grep {$_ eq $function} @subcommand_url) {
    for my $arg (@ARGV) {
      my $var = expand_url_keyword (URL => $arg);
      $arg = $var if $arg ne $var;
    }
  }

  # Expand revision keywords (for -r or --revision options) if necessary
  if (grep {$_ eq $function} @subcommand_rev) {
    my @new_argv = ();

    while (defined (my $arg = shift @ARGV)) {
      if ($arg eq '--revision') {
        # Long --revision option, must be followed by a space before the
        # revision argument
        push @new_argv, $arg;

      } elsif ($arg =~ s/^-r//) {
        # Short -r option, may be followed by the revision argument with or
        # without a space in between
        push @new_argv, '--revision';
        unshift @ARGV, $arg if $arg;

      } else {
        # Other option or argument
        push @new_argv, $arg;
        next;
      }

      # First revision number/keyword
      my $rev1 = '';

      # Get the next argument from the list
      $arg = shift @ARGV;

      if (index ($arg, '{') == 0) {
        # A revision date argument may contain a space. Therefore, it may need
        # the next argument(s) from the list
        while (index ($arg, '}') == -1) {
          my $shift = shift @ARGV;
          last unless $shift;
          $arg     .= ' ' . $shift;
        }

        $arg  =~ s/^(\{.+?\})//;
        $rev1 = $1;

      } else {
        # Other revision argument
        $arg  =~ s/^(\S+?)(?::|$)//;
        $rev1 = $1;
      }

      # The rest of $arg is the second revision number/keyword
      my $rev2 = $arg;
      $rev2 =~ s/^:*//;

      # A revision date argument may contain a space. Therefore, it may need
      # the next argument(s) from the list
      if (index ($rev2, '{') == 0) {
        while (index ($rev2, '}') == -1) {
          my $shift = shift @ARGV;
          last unless $shift;
          $rev2    .= ' ' . $shift;
        }
      }

      # Expand revision keyword if necessary
      if ($rev1 !~ /^(?:\d+|HEAD|BASE|COMMITTED|PREV|\{.+\})$/i or
          $rev2 !~ /^(?:\d+|HEAD|BASE|COMMITTED|PREV|\{.+\})$/i) {
        # Find out the associated URLs by inspecting the argument list
        my $url1 = '';
        my $url2 = '';

        for (@new_argv, @ARGV) {
          my $arg = Fcm::CmUrl->new (URL => $_);
          next unless $arg->is_url;

          if ($url1) {
            $url2 = $arg->url_peg;
            last;

          } else {
            $url1 = $arg->url_peg;
          }
        }

        # Argument list does not contain a URL, try "svn info" on WC
        $url1 = &get_url_of_wc () if not $url1;
        $url2 = $url1 if not $url2;

        # Expand 1st revision keyword if necessary
        $rev1 = expand_rev_keyword (REV => $rev1, URL => $url1)
          if $rev1 !~ /^(?:\d+|HEAD|BASE|COMMITTED|PREV|\{.+\})$/i;

        # Expand 2nd revision keyword if necessary
        $rev2 = expand_rev_keyword (REV => $rev2, URL => $url2)
          if $rev2 and $rev2 !~ /^(?:\d+|HEAD|BASE|COMMITTED|PREV|\{.+\})$/i;
      }

      # Append revision argument to argument list
      push @new_argv, ($rev2 ? $rev1 . ':' . $rev2 : $rev1);
    }

    @ARGV = @new_argv;
  }

  # Expand revision keywords (for peg revision TARGET@REVSION) if necessary
  for (@ARGV) {
    if (m#^(\w+://\S+)@(\S+)$#) {
      my $url = $1;
      my $rev = $2;

      my $new_rev = expand_rev_keyword (URL => $url, REV => $rev, HEAD => 1);

      $_ = $url . '@' . $new_rev if $new_rev ne $rev;
    }
  }

  # List of special sub-commands recognised by FCM
  my %subcommand = (
    ADD       => [qw/add/],
    BRANCH    => [qw/branch br/],
    COMMIT    => [qw/commit ci/],
    CONFLICTS => [qw/conflicts cf/],
    CHECKOUT  => [qw/checkout co/],
    DELETE    => [qw/delete del remove rm/],
    DIFF      => [qw/diff di/],
    MERGE     => [qw/merge/],
    MKPATCH   => [qw/mkpatch/],
    SWITCH    => [qw/switch sw/],
  );

  if (grep {$_ eq $function} @{ $subcommand{ADD} }) {
    cm_add;

  } elsif (grep {$_ eq $function} @{ $subcommand{BRANCH} }) {
    cm_branch;

  } elsif (grep {$_ eq $function} @{ $subcommand{CHECKOUT} }) {
    # Check whether the last argument is a PATH.
    # If so, check whether it is a working copy.
    # Otherwise, check whether the current directory is a working copy.
    # If current working direcory (or PATH) is a working copy, fail the command.
    if (@ARGV) {
      my $arg  = Fcm::CmUrl->new (URL => $ARGV [-1]);
      my $path = $arg->is_url ? cwd () : $ARGV [-1];

      e_report $path, ': already a working copy, abort checkout.'
        if &is_wc ($path);
    }

    # Invoke checkout
    cm_svn ('checkout');

  } elsif (grep {$_ eq $function} @{ $subcommand{COMMIT} }) {
    cm_commit;

  } elsif (grep {$_ eq $function} @{ $subcommand{CONFLICTS} }) {
    cm_conflicts;

  } elsif (grep {$_ eq $function} @{ $subcommand{DELETE} }) {
    cm_delete;

  } elsif (grep {$_ eq $function} @{ $subcommand{DIFF} }) {
    cm_diff;

  } elsif (grep {$_ eq $function} @{ $subcommand{MERGE} }) {
    cm_merge;

  } elsif (grep {$_ eq $function} @{ $subcommand{MKPATCH} }) {
    cm_mkpatch;

  } elsif (grep {$_ eq $function} @{ $subcommand{SWITCH} }) {
    cm_switch;

  } else {
    cm_svn ($function);
  }

}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &Fcm::Cm::cm_add ();
#
# DESCRIPTION
#   This is a wrapper to "svn add". It adds an extra functionality to check
#   for any files or directories reported by "svn status" as not under version
#   control, and to prompt the user whether these files or directories should
#   be added.
# ------------------------------------------------------------------------------

sub cm_add {

  # Print usage message if requested
  if (defined ($ARGV[0]) and grep {$_ eq $ARGV[0]} qw(--help -h)) {
    print <<EOF;
usage: fcm add [options] [args]

Valid options:
  -c [--check]  : Check for any files or directories reported by svn status as
                  not under version control and add them.
  <SVN options> : Standard options to svn add as described below ...

EOF

    &run_command ([qw/svn add --help/], PRINT => 1, METHOD => 'exec');
  }

  if (defined ($ARGV[0]) and grep {$_ eq $ARGV[0]} qw(--check -c)) {
    # The --check option is specified, add any new files
    # Execute "svn status", print lines starting with a "?"
    my $pat    = '^\?.{4}\s*';
    my @status = grep /$pat.*/, &run_command ([qw/svn status/], METHOD => 'qx');
    print @status if @status;

    # Get list of "?" files
    my @files  = map {chomp; s/$pat//; $_} @status;
    my $reply  = '';

    # Execute "add" command depending on user reply
    for my $file (@files) {
      # Get a user reply, unless previous reply is "a" for "all"
      $reply = &main::get_input (
        TITLE   => 'fcm add',
        MESSAGE => "Add file '$file'?",
        TYPE    => 'yna',
        DEFAULT => 'n',
      ) unless $reply eq "a";

      # Add current $file if reply is "y" for "yes" or "a" for "all"
      &run_command ([qw/svn add/, $file]) if $reply =~ /^[ya]$/;
    }

  } else {
    # The --check option is not specified, just call "svn add"
    cm_svn ("add");
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &Fcm::Cm::cm_branch ();
#
# DESCRIPTION
#   This is a FCM command to check information, create or delete a branch in
#   a Subversion repository.
# ------------------------------------------------------------------------------

sub cm_branch {
  my $usage = <<EOF;
branch: Create, delete or display information of a branch
usage: 1. fcm branch [--info] [OPTIONS] [TARGET]
       2. fcm branch --delete [OPTIONS] [TARGET]
       3. fcm branch --create [OPTIONS] [SOURCE]
       4. fcm branch --list   [OPTIONS] [SOURCE]

  1. --info or -i: Display information about a branch. This is the default
     option if --create, --delete and --list are not specified.

  2. --delete or -d: Delete a branch.

  3. --create or -c: Create a new branch from SOURCE. The --name option must be
     used to specify a short name for the new branch.

  4. --list or -l: List all the branches owned by the current user in SOURCE. If
     the --user option is specified with a list of users, list all the branches
     owned by these users instead of the current user.

  TARGET (and SOURCE) can be an URL or a Subversion working copy. Otherwise,
  the current working directory must be a working copy. For --info and
  --delete, the specified URL (or the URL of the working copy) must be a URL
  under a valid branch in a standard FCM project. For --create and --list, it
  must be a URL under a standard FCM project.

Valid options with --info and --delete:
  -v [--verbose]        : Print extra information.
  -a [--show-all]       : Set --show-children, --show-other and --show-siblings.
  --show-children       : Report children of the current branch.
  --show-other          : Report custom/ reverse merges into the current branch.
  --show-siblings       : Report merges with siblings of the current branch.

Valid options with --delete and --create:
  --non-interactive     : Do no interactive prompting. This option implies
                          --svn-non-interactive.
  --password arg        : Specify a password for write access to the repository.
  --svn-non-interactive : Do no interactive prompting at commit time. This
                          option is implied by --non-interactive.

Valid options with --create and --list:
  -r [--revision] arg   : Specify the operative revision of the SOURCE for
                          creating the branch.

Valid options with --create:
  --branch-of-branch    : If this option is specified and the SOURCE is a
                          branch, it will create a new branch from the SOURCE
                          branch. Otherwise, the branch is created from the
                          trunk.
  -k [--ticket] arg     : Specify one (or more) Trac ticket. If specified, the
                          command will add to the commit log the line "Relates
                          to ticket #<ticket>". Multiple tickets can be set by
                          specifying this option multiple times, or by
                          specifying the tickets in a comma-separated list.
  -n [--name] arg       : Specify a short name for the branch, which should
                          contain only word characters, i.e. [A-Za-z0-9_].
  --rev-flag arg        : Specify a flag for determining the prefix of the
                          branch name. The flag can be the the string "NORMAL",
                          "NUMBER" or "NONE".  "NORMAL" is the default
                          behaviour, in which the branch name will be prefixed
                          with a Subversion revision number if the revision is
                          not associated with a registered FCM revision
                          keyword. If the revision is registered with a FCM
                          revision keyword, the keyword will be used in place
                          of the number. If "NUMBER" is specified, the branch
                          name will always be prefixed with a Subversion
                          revision number. If "NONE" is specified, the branch
                          name will not be prefixed by a revision number or
                          keyword.
  -t [--type] arg       : Specify the type of the branch to be created. It must
                          be one of the following:
                            DEV::USER   - a development branch for the user
                            DEV::SHARE  - a shared development branch
                            DEV         - same as DEV::USER
                            TEST::USER  - a test branch for the user
                            TEST::SHARE - a shared test branch
                            TEST        - same as TEST::USER
                            PKG::USER   - a package branch for the user
                            PKG::SHARE  - a shared package branch
                            PKG::CONFIG - a configuration branch
                            PKG::REL    - a release branch
                            PKG         - same as PKG::USER
                            CONFIG      - same as PKG::CONFIG
                            REL         - same as PKG::REL
                            SHARE       - same as DEV::SHARE
                            USER        - same as DEV::USER
                          If not specified, the default is to create a
                          development branch for the current user, i.e.
                          DEV::USER.

Valid options with --list:
  -u [--user] arg       : Specify a colon-separated list of users. List branches
                          owned by these users instead of the current user.
  -v [--verbose]        : Print Subversion URL instead of FCM URL keywords.
EOF

  # Print usage message if requested
  if (defined ($ARGV[0]) and grep {$_ eq $ARGV[0]} qw(--help -h)) {
    print $usage;
    return 1;
  }

  # Process command line options
  # ----------------------------------------------------------------------------
  my (
    $info,
    $delete,
    $create,
    $list,
    $branch_of_branch,
    $name,
    $non_interactive,
    $password,
    $rev,
    $rev_flag,
    $show_all,
    $show_children,
    $show_other,
    $show_siblings,
    $svn_non_interactive,
    @tickets,
    $type,
    @userlist,
    $verbose,
  );
  GetOptions (
    'info|i'              => \$info,
    'delete|d'            => \$delete,
    'create|c'            => \$create,
    'list|l'              => \$list,
    'branch-of-branch'    => \$branch_of_branch,
    'name|n=s'            => \$name,
    'non-interactive'     => \$non_interactive,
    'password=s'          => \$password,
    'revision|r=s'        => \$rev,
    'rev-flag=s'          => \$rev_flag,
    'show-all|a'          => \$show_all,
    'show-children'       => \$show_children,
    'show-other'          => \$show_other,
    'show-siblings'       => \$show_siblings,
    'svn-non-interactive' => \$svn_non_interactive,
    'ticket|k=s'          => \@tickets,
    'type|t=s'            => \$type,
    'user|u=s'            => \@userlist,
    'verbose|v'           => \$verbose,
  );

  my $num_options = 0;
  $num_options++ if defined $info;
  $num_options++ if defined $delete;
  $num_options++ if defined $create;
  $num_options++ if defined $list;

  # Report invalid usage
  # ----------------------------------------------------------------------------
  e_report $usage if $num_options > 1;

  # Get URL of repository or branch
  # ----------------------------------------------------------------------------
  my $url;
  if ($ARGV[0]) {
    $url = Fcm::CmUrl->new (URL => $ARGV[0]);

    if (not $url->is_url) {
      # An argument is specified and is not a URL
      # Assume that it is a path with a working copy
      if (&is_wc ($ARGV[0])) {
        $url = Fcm::CmUrl->new (URL => &get_url_of_wc ($ARGV[0]));

      } else {
        e_report $ARGV[0], ': is not a working copy, abort.';
      }
    }

  } else {
    # An argument is not specified
    # Assume that the current directory is a working copy
    if (&is_wc ()) {
      $url = Fcm::CmUrl->new (URL => &get_url_of_wc ());

    } else {
      e_report 'The current directory is not a working copy, please specify a ',
               'URL or a path to a working copy, abort.';
    }
  }

  # Ensure $url->url_peg is a URL of a standard FCM project
  e_report $url->url_peg, ': not a URL of a standard FCM project, abort.'
    if not $url->project_url;

  if ($create) {
    # The --create option is specified, create a branch
    # --------------------------------------------------------------------------

    # Check branch type flags
    if ($type) {
      $type = uc ($type);

      if ($type =~ /^(USER|SHARE)$/) {
        $type = 'DEV::' . $1;

      } elsif ($type =~ /^(CONFIG|REL)$/) {
        $type = 'PKG::' . $1;

      } elsif ($type =~ /^(DEV|TEST|PKG)$/) {
        $type = $1 . '::USER';

      } elsif ($type !~ /^(?:DEV|TEST|PKG)::(?:USER|SHARE)$/ and
               $type !~ /^PKG::(?:CONFIG|REL)/) {
        e_report $type, ': is not a valid type flag, abort.';
      }

    } else {
      $type = 'DEV::USER';
    }

    # Check branch name
    e_report 'The option --name must be used to specify a branch name, abort.'
      if not $name;

    e_report $name, ': invalid characters in name, abort.' if $name !~ /^\w+$/;

    # Check revision flag is valid
    if ($rev_flag) {
      $rev_flag = uc ($rev_flag);

      e_report $rev_flag, ': invalid argument to the --rev-flag option, abort.'
        if $rev_flag !~ /^(?:NORMAL|NUMBER|NONE)$/;

    } else {
      $rev_flag = 'NORMAL';
    }

    # Handle multiple tickets
    @tickets = split (/,/, join (',', @tickets));
    s/^#// for (@tickets);
    @tickets = sort {$a <=> $b} @tickets;

    # Determine whether to create a branch of a branch
    $url->branch ('trunk') unless $branch_of_branch;

    # Create the branch
    my $branch = Fcm::CmBranch->new;
    $branch->create (
      SRC                 => $url,
      TYPE                => $type,
      NAME                => $name,
      PASSWORD            => $password,
      REV_FLAG            => $rev_flag,
      TICKET              => \@tickets,
      REV                 => $rev,
      NON_INTERACTIVE     => $non_interactive,
      SVN_NON_INTERACTIVE => $svn_non_interactive,
    );

  } elsif ($list) {
    # The option --list is specified
    # List branches owned by current or specified users
    # --------------------------------------------------------------------------
    # Get URL of the project "branches/" sub-directory
    $url->subdir ('');
    $url->branch ('');
    my @list = map {$_, 1} $url->branch_list ($rev);

    if (@userlist) {
      # Sort list of users
      @userlist = sort (split /:/, join (':', @userlist));

    } else {
      # No user specified, add currrent user to list
      push @userlist, $ENV{LOGNAME} unless @userlist;
    }

    # Filter branches matching user list
    my @branches;
    for my $branch (@list) {
      next unless $branch =~ m#/([^/]+)/[^/]+/*$#;

      my $user = $1;

      push @branches, $branch if grep {$user eq $_} @userlist;
    }

    # Output, number of branches found
    print scalar (@branches), ' ',
          (scalar (@branches) > 1 ? 'branches' : 'branch'), ' found for ',
          join (', ', @userlist), ' in ', $url->project_url_peg,
          ($rev ? (' at r', $rev) : ()), "\n";

    if (@branches) {
      # Output the URL of each branch
      if (not $verbose) {
        my $project = $url->project_url;
        my $keyword = &get_url_keyword (URL => $project);
        @branches = map {s#^$project/+branches#$keyword-br#; $_} @branches
          if defined $keyword;
      }
      @branches = map {$_ . "\n"} sort @branches;
      print @branches;

    } else {
      # No branch found, exit with an error code
      exit 1;
    }

  } else {
    # The option --info or --delete is specified
    # Report branch information (and/or delete a branch)
    # --------------------------------------------------------------------------
    # Set verbose level
    &main::cfg->verbose ($verbose ? 1 : 0);

    # Set up the branch, report any error
    my $branch = Fcm::CmBranch->new (URL => $url->url_peg);
    e_report $branch->url_peg, ': not a branch, abort.' unless $branch->branch;

    e_report $branch->url_peg, ': does not exist, abort.'
      unless $branch->url_exists;

    # Remove the sub-directory part of the URL
    $branch->subdir ('');

    # Report branch info
    $branch->display_info (
      SHOW_CHILDREN => ($show_all || $show_children),
      SHOW_OTHER    => ($show_all || $show_other   ),
      SHOW_SIBLINGS => ($show_all || $show_siblings),
    );

    # Delete branch if --delete is specified
    $branch->del (
      PASSWORD            => $password,
      NON_INTERACTIVE     => $non_interactive,
      SVN_NON_INTERACTIVE => $svn_non_interactive,
    ) if $delete;
  }

}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &Fcm::Cm::cm_commit ();
#
# DESCRIPTION
#   This is a FCM wrapper to the "svn commit" command.
# ------------------------------------------------------------------------------

sub cm_commit {

  # Print usage message if requested
  if (defined ($ARGV[0]) and grep {$_ eq $ARGV[0]} qw(--help -h)) {
    print <<EOF;
commit (ci): Send changes from your working copy to the repository.
usage: fcm commit [PATH]

  Invoke your favourite editor to prompt you for a commit log message. Send
  changes from your working copy to the repository. Update your working copy
  following the commit.

Valid options:
  --dry-run             : Allows you to add to the commit message without
                          committing.
  --svn-non-interactive : Do no interactive prompting at commit time.
  --password arg        : Specify a password ARG.
EOF
    return 1;
  }

  my ($dry_run, $svn_non_interactive, $password);
  GetOptions (
    'dry-run'             => \$dry_run,
    'svn-non-interactive' => \$svn_non_interactive,
    'password'            => \$password,
  );

  # The remaining argument is the path to a working copy
  my ($path) = @ARGV;

  if ($path) {
    # Check that specified path exists
    e_report $path, ': does not exist, abort.' if not -e $path;

  } else {
    # No argument specified, use current working directory
    $path = cwd ();
  }

  # Make sure we are in a working copy
  e_report $path, ': not a working copy, abort.' if not &is_wc ($path);

  # Make sure we are at the top level of the working copy
  # (otherwise we might miss any template commit message)
  my $dir = &get_wct ($path);

  if ($dir ne cwd ()) {
    chdir $dir or die 'Cannot change directory to: ', $dir;
    print 'Committing changes from ', $dir, ' ...', "\n";
  }

  # Get update status of working copy
  # Check working copy files are not in conflict, missing, or out of date
  my @status = &run_command ([qw/svn status --show-updates/], METHOD => 'qx');
  unless (defined $dry_run) {
    my (@conflict, @missing, @outdate);

    for (@status) {
      if (/^C/) {
        push @conflict, $_;
        next;
      }

      if (/^!/) {
        push @missing, $_;
        next;
      }

      if (/^.{7}\*/) {
        push @outdate, $_;
        next;
      }

      # Check that all files which have been added have the svn:executable
      # property set correctly (in case the developer adds a script before they
      # remember to set the execute bit)
      next unless /^A.{7} *\d+ +(.*)/;
      my $file = $1;

      next unless -f $file;
      my @command = (-x $file)
                    ? (qw/svn propset -q svn:executable */, $file)
                    : (qw/svn propdel -q svn:executable/  , $file);
      &run_command (\@command);
    }

    # Abort commit if files are in conflict, missing, or out of date
    if (@conflict or @missing or @outdate) {
      w_report 'File(s) in conflict:', "\n", @conflict if @conflict;
      w_report 'File(s) missing:'    , "\n", @missing  if @missing;
      w_report 'File(s) out of date:', "\n", @outdate  if @outdate;
      e_report 'Abort commit.';
    }
  }

  # Read in any existing message
  my $ci_mesg = Fcm::CmCommitMessage->new ();
  $ci_mesg->read_file;

  # Execute "svn status" for a list of changed items
  @status = grep !/^\?/, &run_command ([qw/svn status/], METHOD => 'qx');

  # Abort if there is no change in the working copy
  if (not @status) {
    print 'No change in working copy, abort.', "\n";
    return;
  }

  # Get associated URL of current working copy
  my $url = Fcm::CmUrl->new (URL => &get_url_of_wc ());

  # Include URL, or project, branch and sub-directory info in @status
  unshift @status, "\n";

  if ($url->project and $url->branch) {
    unshift @status, (
      '[Project: ' . $url->project                           . ']' . "\n",
      '[Branch : ' . $url->branch                            . ']' . "\n",
      '[Sub-dir: ' . ($url->subdir ? $url->subdir : '<top>') . ']' . "\n",
    );

  } else {
    unshift @status, '[URL: ' . $url->url . ']' . "\n";
  }

  # Use a temporary file to store the final commit log message
  $ci_mesg->ignore_mesg (@status);
  my $logfile = $ci_mesg->edit_file (TEMP => 1);

  # Check with the user to see if he/she wants to go ahead
  my $reply = 'n';
  if (not defined $dry_run) {
    # Add extra warning for trunk commit
    my $mesg = $url->is_trunk
      ? "\n" .
        '*** WARNING: YOU ARE COMMITTING TO THE TRUNK.' . "\n" .
        '*** Please ensure that your change conforms to your project\'s ' .
        'working practices.' . "\n\n"
      : '';
    $mesg   .= 'Would you like to commit this change?';

    # Prompt the user
    $reply = &main::get_input (
      TITLE   => 'fcm commit',
      MESSAGE => $mesg,
      TYPE    => 'yn',
      DEFAULT => 'n',
    );
  }

  if ($reply eq 'y') {
    # Commit the change if user replies "y" for "yes"
    my @command = (
      qw/svn commit -F/, $logfile,
      ($svn_non_interactive  ? '--non-interactive'       : ()),
      (defined $password     ? ('--password', $password) : ()),
    );
    my $rc;
    &run_command (\@command, RC => \$rc, ERROR => 'warn');

    if ($rc) {
      # Commit failed
      # Write temporary commit log content to commit log message file
      $ci_mesg->write_file;

      # Fail the command
      e_report;
    }

    # Remove commit message file
    unlink $ci_mesg->file;

    # Update the working copy
    print 'Performing update to make sure your working copy is at this new ',
          'revision ...', "\n";
    &run_command ([qw/svn update/]);

  } else {
    # Abort commit if dry run or user replies "n" for "no"
    w_report 'Commit aborted by user.' unless $dry_run;

    # Write temporary commit log content to commit log message file
    $ci_mesg->write_file;
  }

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &Fcm::Cm::cm_conflicts ();
#
# DESCRIPTION
#   This is a FCM command for resolving conflicts within working copy using a
#   graphical merge tool.
# ------------------------------------------------------------------------------

sub cm_conflicts {

  # Print usage message if requested
  if (defined ($ARGV[0]) and grep {$_ eq $ARGV[0]} qw(--help -h)) {
    print <<EOF;
conflicts: Use graphical tool to resolve any conflicts within your working copy.
usage: fcm conflicts [PATH]

  Invoke the xxdiff graphical merge tool to help you resolve conflicts in your
  working copy. It prompts you to run "svn resolved" each time you have
  resolved the conflicts in a file.
EOF
    return 1;
  }

  # Path to the working copy
  my $path = $ARGV[0];
  $path    = cwd () if not $path;

  # Check for any files with conflicts
  my @status = grep /^C.{4} *(.*)/, &run_command (
    [qw/svn st/, ($path eq cwd () ? () : $path)], METHOD => 'qx',
  );
  my @files  = map {m/^C.{4} *(.*)/; $1} @status;

  # Save current working directory
  my $topdir = cwd ();

  for my $file (@files) {
    # Print name of file in conflicts
    print "Conflicts in file: $file\n";

    # Determine directory and base name of file in conflicts
    my $base = basename $file;
    my $dir  = dirname $file;

    # Change to container directory of file in conflicts
    chdir File::Spec->catfile ($topdir, $dir) or die "Directory change to $dir failed";

    # Use "svn info" to determine conflict marker files
    my @info = &run_command ([qw/svn info/, $base], METHOD => 'qx');

    # Ignore if $base is a binary file
    if (-B $base) {
      w_report $base,
               ': ignoring binary file, please resolve conflicts manually.';
      next;
    }

    # Get conflicts markers files
    my ($older, $mine, $yours);

    for (@info) {
      $older = $1 if (/^Conflict Previous Base File: (.*)/);
      $mine  = $1 if (/^Conflict Previous Working File: (.*)/);
      $yours = $1 if (/^Conflict Current Base File: (.*)/);
    }

    if ((stat $base)[9] > (stat $mine)[9]) {
      # If $base is newer, it may contain saved changes
      my $reply = &main::get_input (
        TITLE   => 'fcm conflicts',
        MESSAGE => 'Existing changes in ' . $base . ' will be overwritten.' .
                   "\n" . 'Do you wish to continue?',
        TYPE    => 'yn',
        DEFAULT => 'n',
      );

      next if $reply ne 'y';
    }

    # Launch "xxdiff" to allow user to perform graphical merging
    my $xxdiffrc;
    my @command  = (qw/xxdiff -m -M/, $base, qw/-O -X/, $mine, $older, $yours);
    my ($decision) = &run_command (
      \@command, METHOD => 'qx', RC => \$xxdiffrc, ERROR => 'ignore',
    );
    die &get_command_string (\@command), ' failed' if $xxdiffrc and ! $decision;
    chomp $decision;

    # Perform different actions depending on the user's decision
    if ($decision eq "NODECISION") {
      print "No decision taken\n";

    } elsif ($decision eq "MERGED" and $xxdiffrc != 0) {
      print "Merge conflicts were not all resolved\n";

    } else {
      # User has MERGED, ACCEPTED or REJECTED all changes
      if ($decision eq "MERGED") {
        print "All merge conflicts resolved\n";

      } else {
        print "You have chosen to $decision all the changes\n";
      }

      # Prompt user to run "svn resolved" on the file
      my $reply = &main::get_input (
        TITLE   => 'fcm conflicts',
        MESSAGE => 'Would you like to run "svn resolved"?',
        TYPE    => 'yn',
        DEFAULT => 'n',
      );

      # If reply is "yes"...
      &run_command ([qw/svn resolved/, $base]) if $reply eq 'y';
    }
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &Fcm::Cm::cm_delete ();
#
# DESCRIPTION
#   This is a wrapper to "svn delete". It adds an extra functionality to check
#   for any files or directories reported by "svn status" as missing, and to
#   prompt the user whether these files or directories should be deleted.
# ------------------------------------------------------------------------------

sub cm_delete {

  # Print usage message if requested
  if (defined ($ARGV[0]) and grep {$_ eq $ARGV[0]} qw(--help -h)) {
    print <<EOF;
usage: fcm delete [options] [args]

Valid options:
  -c [--check]  : Check for any files or directories reported by svn status as
                  missing and delete them.
  <SVN options> : Standard options to svn delete as described below ...

EOF

    &run_command ([qw/svn delete --help/], PRINT => 1, METHOD => 'exec');
  }

  if (defined ($ARGV[0]) and grep {$_ eq $ARGV[0]} qw(--check -c)) {
    # The --check option is specified, delete any missing files
    # Execute "svn status", print lines starting with a "!"
    my $pat    = '^!.{4}\s*';
    my @status = grep /$pat.*/, &run_command ([qw/svn status/], METHOD => 'qx');
    print @status if @status;

    # Get list of "!" files
    my @files  = map {chomp; s/$pat//; $_} @status;
    my $reply  = '';

    # Execute "delete" command depending on user reply
    for my $file (@files) {
      # Get a user reply, unless previous reply is "a" for "all"
      $reply = &main::get_input (
        TITLE   => 'fcm delete',
        MESSAGE => "Delete file '$file'?",
        TYPE    => 'yna',
        DEFAULT => 'n',
      ) unless $reply eq "a";

      # Delete current $file if reply is "y" for "yes" or "a" for "all"
      &run_command ([qw/svn delete/, $file]) if $reply =~ /^[ya]$/;
    }

  } else {
    # The --check option is not specified, just call "svn delete"
    cm_svn ("delete");
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &Fcm::Cm::cm_diff ();
#
# DESCRIPTION
#   This is a wrapper to "svn diff". It adds two extra functionalities. The
#   first one allows the command to show differences relative to the base of
#   the branch. The second one allows differences to be displayed via a
#   graphical tool.
# ------------------------------------------------------------------------------

sub cm_diff {

  # Print usage message if requested
  if (defined ($ARGV[0]) and grep {$_ eq $ARGV[0]} qw(--help -h)) {
    print <<EOF;
usage: 1. fcm diff --branch [OPTIONS] [TARGET]
       2. fcm diff [OPTIONS] [ARGS]

  1. --branch or -b: Show differences relative to the base of the target branch,
     i.e. the changes available for merging from the target branch into its
     parent. If TARGET is specified, it must either be a URL or a working copy.
     Otherwise, the target is the the current directory which must be a working
     copy. The target URL must be a branch in a standard FCM project.

  2. See description of "svn diff" below.

Valid options:
  -g [--graphical] : Use a graphical diff tool to display the differences. This
                     option should not be used in combination with --diff-cmd.
  <SVN options>    : Standard options to "svn diff" as described below.

Valid options with --branch:
  --diff-cmd arg        : As described below in the help for "svn diff".
  -g [--graphical]      : As described above.
  -t [--trac]           : If TARGET is a URL, use Trac to display the diff.
  --wiki                : If TARGET is a URL, print Trac link for the diff.
  -x [--extensions] arg : As described below in the help for "svn diff".

EOF

    &run_command ([qw/svn diff --help/], PRINT => 1, METHOD => 'exec');
  }

  # Set up environment for graphical diff
  # Use environment variable if set, otherwise use default setting
  my $env = 'FCM_GRAPHIC_DIFF';
  $ENV{$env} = &main::cfg->setting (qw/TOOL GRAPHIC_DIFF/)
    unless exists $ENV{$env} or not &main::cfg->setting (qw/TOOL GRAPHIC_DIFF/);

  # Check for the --branch options
  # ----------------------------------------------------------------------------
  my $branch = grep {$_ eq '-b' or $_ eq '--branch'} @ARGV;

  if (not $branch) {
    # The --branch option not specified, just call "svn diff"
    # Convert the --graphical to qw/--diff-cmd fcm_graphical_diff/
    @ARGV = map {
      ($_ eq '-g' or $_ eq '--graphical')
      ? (qw/--diff-cmd fcm_graphic_diff/)
      : $_
    } @ARGV;

    # Execute the command
    cm_svn ('diff');
  }

  # The --branch option is specified
  # ----------------------------------------------------------------------------

  # Determine whether the --graphical option is specified,
  # if so set the appropriate command
  # ----------------------------------------------------------------------------
  my ($diff_cmd, $extensions, $graphical, $trac, $wiki);
  GetOptions (
    'b|branch'       => \$branch,
    'diff-cmd=s'     => \$diff_cmd,
    'x|extensions=s' => \$extensions,
    'g|graphical'    => \$graphical,
    't|trac'         => \$trac,
    'wiki'           => \$wiki,
  );

  my @diff_cmd = ();
  
  if ($graphical) {
    @diff_cmd = (qw/--diff-cmd fcm_graphic_diff/);

  } elsif ($diff_cmd) {
    @diff_cmd = ('--diff-cmd', $diff_cmd);

    push @diff_cmd, '--extensions', split (/\s+/, $extensions) if $extensions;
  }

  # The remaining argument should either be a URL or a PATH
  my ($url_arg, $path_arg);

  if (@ARGV) {
    my $arg = Fcm::CmUrl->new (URL => $ARGV[0]);

    if ($arg->is_url) {
      $url_arg = $ARGV[0];

    } else {
      $path_arg = $ARGV[0];
    }
  }

  # Get repository and branch information
  # ----------------------------------------------------------------------------
  my ($url, $path);
  if (defined $url_arg) {
    # If a URL is specified, get repository and branch information from it
    $url = Fcm::CmBranch->new (URL => $url_arg);

  } else {
    # Get repository and branch information from the specified path or the
    # current directory if it is a working copy
    $path = $path_arg ? $path_arg : cwd ();
    e_report $path, ': not a working copy, abort.' unless &is_wc ($path);

    $url  = Fcm::CmBranch->new (URL => &get_url_of_wc ($path));
  }

  # Check that URL is a standard FCM branch
  e_report $url->url_peg, ': not a standard FCM branch, abort.'
    unless $url->is_branch;

  # Save and remove sub-directory part of the URL
  my $subdir = $url->subdir ();
  $url->subdir ('');

  # Check that $url exists
  e_report $url->url_peg, ': not a valid URL, abort.' unless $url->url_exists;

  # Compare current branch with its parent
  # ----------------------------------------------------------------------------
  my $parent = Fcm::CmBranch->new (URL => $url->parent->url);
  $parent->pegrev ($url->pegrev) if $url->pegrev;

  e_report $parent->url, ': branch parent no longer exists',
           ($parent->pegrev ? ' at ' . $parent->pegrev : ''), ', abort.'
    unless $parent->url_exists;

  my $base = $parent->base_of_merge_from ($url);

  # Ensure the correct diff (syntax) is displayed
  # ----------------------------------------------------------------------------
  # Reinstate the sub-tree part into the URL
  $url->subdir ($subdir);
  $base->subdir ($subdir);

  # Ensure the branch URL has a peg revision
  $url->pegrev ($url->svninfo (FLAG => 'Last Changed Rev')) if not $url->pegrev;

  if ($trac or $wiki) {
    # Trac/wiki
    # --------------------------------------------------------------------------
    if (not $url_arg) {
      if (&run_command ([qw/svn status/], METHOD => 'qx')) {
        w_report 'WARNING: the working copy at "', ($path_arg ? $path_arg : '.'),
                 '" contains local changes, which cannot be displayed in Trac.';
      }
    }

    # Trac wiki syntax
    my $wiki_syntax = 'diff:' . $base->path_peg . '//' . $url->path_peg;

    if ($wiki) {
      # Print Trac wiki syntax only
      print $wiki_syntax, "\n";

    } else { # if $trac
      # Use Trac to view "diff"
      my $browser  = &main::cfg->setting (qw/MISC WEB_BROWSER/);
      die 'ERROR: web browser not set, abort' if not $browser;

      my $trac_url = &get_browser_url (URL => $url->project_url);
      e_report 'ERROR: ', $url->project_url,
               ': not associated with a Trac URL, abort.'
        if not $trac_url;

      $trac_url =~ s#/browser/.*$#/intertrac/$wiki_syntax#;

      &run_command ([$browser, $trac_url], METHOD => 'exec', PRINT => 1);
    }

  } else {
    # Execute the "diff" command
    # --------------------------------------------------------------------------
    my @command = (
      qw/svn diff/, @diff_cmd,
      '--old', $base->url_peg,
      '--new', ($url_arg ? $url->url_peg : ($path_arg ? $path_arg : '.')),
    );
    &run_command (\@command, PRINT => 1);
  }
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &Fcm::Cm::cm_merge ();
#
# DESCRIPTION
#   This is a wrapper to "svn merge".
# ------------------------------------------------------------------------------

sub cm_merge {

  # Print usage message if requested
  if (defined ($ARGV[0]) and grep {$_ eq $ARGV[0]} qw(--help -h)) {
    print <<EOF;
merge: Merge changes from a source into your working copy.
usage: 1. fcm merge SOURCE
       2. fcm merge --custom  --revision N[:M] SOURCE
          fcm merge --custom  URL[\@REV1] URL[\@REV2]
       3. fcm merge --reverse --revision [M:]N

  1. If neither --custom nor --reverse is specified, the command merges changes
     automatically from SOURCE into your working copy. SOURCE must be a valid
     URL[\@REV] of a branch in a standard FCM project. The base of the merge
     will be calculated automatically based on the common ancestor and latest
     merge information between the SOURCE and the branch of the working copy.

  2. If --custom is specified, the command can be used in two forms.
  
     In the first form, it performs a custom merge from the specified
     changeset(s) of SOURCE into your working copy. SOURCE must be a valid
     URL[\@REV] of a branch in a standard FCM project. If a single revision is
     specified, the merge delta is (N - 1):N of SOURCE. Otherwise, the merge
     delta, is N:M of SOURCE, where N < M.
     
     In the second form, it performs a custom merge using the delta between the
     two specified branch URLs. For each URL, if a peg revision is not
     specified, the command will peg the URL with its last changed revision.

  3. If --reverse is specified, the command performs a reverse merge of the
     changeset(s) specified by the --revision option. If a single revision is
     specified, the merge delta is N:(N - 1). Otherwise, the merge delta is
     M:N, where M > N. Note that you do not have to specify a SOURCE for a
     reverse merge, because the SOURCE should always be the branch your working
     copy is pointing to.
  
  The command provide a commit log message template following the merge.

Valid options:
  --dry-run          : Try operation but make no changes.
  --non-interactive  : Do no interactive prompting.
  -r [--revision] arg: Specify a (range of) revision number(s).
  --verbose          : Print extra information.
EOF
    return 1;
  }

  # Options
  # ----------------------------------------------------------------------------
  my ($custom, $dry_run, $non_interactive, $reverse, $rev, $verbose);
  GetOptions (
    'custom'          => \$custom,
    'dry-run'         => \$dry_run,
    'non-interactive' => \$non_interactive,
    'reverse'         => \$reverse,
    'revision|r=s'    => \$rev,
    'verbose|v'       => \$verbose,
  );

  # Find out the URL of the working copy
  # ----------------------------------------------------------------------------
  my ($target, $wct);
  if (&is_wc ()) {
    $wct = &get_wct ();

    if ($wct ne cwd ()) {
      print 'Change directory to top of working copy: ', $wct, "\n";
      chdir $wct or die 'Cannot change directory to: ', $wct;
    }

    $target = Fcm::CmBranch->new (URL => &get_url_of_wc ($wct));

  } else {
    e_report 'The current directory is not a working copy, abort.';
  }

  e_report 'Your working copy URL does not exist at the HEAD revision, abort.'
    unless $target->url_exists;

  # The target must be at the top of a branch
  # $subdir will be used later to determine whether the merge is allowed or not
  my $subdir = $target->subdir;
  $target->subdir ('') if $subdir;

  # Check for any local modifications
  # ----------------------------------------------------------------------------
  return
    if ! ($dry_run or $non_interactive) and &_abort_modified_wc ('fcm merge');

  # Determine the SOURCE URL
  # ----------------------------------------------------------------------------
  my $source;

  if ($reverse) {
    # Reverse merge, the SOURCE is the the working copy URL
    $source = Fcm::CmBranch->new (URL => $target->url);

  } else {
    # Automatic/custom merge, argument 1 is the SOURCE of the merge
    my $source_url = shift (@ARGV);
    e_report 'Error: argument 1 must be the URL/name of a source branch in ',
             'automatic/custom mode, abort.'
      if not $source_url;

    $source = &_construct_branch_url ($source_url, $target);
  }

  # Parse the revision option
  # ----------------------------------------------------------------------------
  my @revs;
  if ($reverse or $custom) {
    if ($reverse and not $rev) {
      e_report 'Error: a revision (range) must be specified with ',
               '--revision in reverse mode, abort.'
    }

    @revs = split (/:/, $rev) if $rev;
  }

  # Determine the merge delta and the commit log message
  # ----------------------------------------------------------------------------
  my (@delta, $mesg);
  my $separator = '-' x 80 . "\n";

  if ($reverse) {
    # Reverse merge
    # --------------------------------------------------------------------------
    if (@revs == 1) {
      $revs[1] = ($revs[0] - 1);

    } else {
      @revs = sort {$b <=> $a} @revs;
    }

    $source->pegrev ($source->svninfo (FLAG => 'Last Changed Rev'))
      unless $source->pegrev;
    $source->subdir ($subdir);

    # "Delta" of the "svn merge" command
    @delta = ('-r' . $revs[0] . ':' . $revs[1], $source->url_peg);

    # Template message
    $mesg = 'Reversed r' . $revs[0] .
            (($revs[1] < $revs[0] - 1) ? ':' . $revs[1] : '') . ' of ' .
            $source->path . "\n";

  } elsif ($custom) {
    # Custom merge
    # --------------------------------------------------------------------------
    if (@revs) {
      # Revision specified
      # ------------------------------------------------------------------------
      # Only one revision N specified, use (N - 1):N as the delta
      unshift @revs, ($revs[0] - 1) if @revs == 1;

      $source->pegrev ($source->svninfo (FLAG => 'Last Changed Rev'))
        unless $source->pegrev;
      $source->subdir ($subdir);
      $target->subdir ($subdir);

      # "Delta" of the "svn merge" command
      @delta = ('-r' . $revs[0] . ':' . $revs[1], $source->url_peg);

      # Template message
      $mesg = 'Custom merge into ' . $target->path . ': r' . $revs[1] .
              ' cf. r' . $revs[0] . ' of ' . $source->path_peg . "\n";

    } else {
      # Revision not specified
      # ------------------------------------------------------------------------
      # Get second source URL
      my $source2_url = shift (@ARGV);
      e_report 'Error: argument 2 must be the URL/name of a source branch in ',
               'custom mode when --revision is not specified, abort.'
        if not $source2_url;

      my $source2 = &_construct_branch_url ($source2_url, $target);

      $source->pegrev  ($source->svninfo  (FLAG => 'Last Changed Rev'))
        unless $source->pegrev;
      $source2->pegrev ($source2->svninfo (FLAG => 'Last Changed Rev'))
        unless $source2->pegrev;
      $source->subdir  ($subdir);
      $source2->subdir ($subdir);
      $target->subdir  ($subdir);

      # "Delta" of the "svn merge" command
      @delta = ($source->url_peg, $source2->url_peg);

      # Template message
      $mesg = 'Custom merge into ' . $target->path . ': ' . $source->path_peg .
              ' cf. ' . $source2->path_peg . "\n";
    }

  } else {
    # Automatic merge
    # --------------------------------------------------------------------------
    # Check to ensure source branch is not the same as the target branch
    e_report 'Error: cannot merge ', $source->branch,
             ' to its own working copy, abort.'
      if $source->branch eq $target->branch;

    # Only allow the merge if the source and target are "directly related"
    # --------------------------------------------------------------------------
    my $anc = $target->ancestor ($source);
    e_report 'Error: source and target are not directly related' unless
      ($anc->url eq $target->url and $anc->url_peg eq $source->parent->url_peg)
      or
      ($anc->url eq $source->url and $anc->url_peg eq $target->parent->url_peg)
      or
      ($anc->url eq $source->parent->url and $anc->url eq $target->parent->url);

    # Check for available merges from the source
    # --------------------------------------------------------------------------
    my @revs = $target->avail_merge_from ($source, 1);

    if (@revs) {
      print 'Available Merge', (@revs > 1 ? 's' : ''), ' From ',
            $source->path_peg, ':';

      if ($verbose) {
        # Verbose mode, print log messages of available merges
        print "\n";

        for (@revs) {
          print $separator, $source->display_svnlog ($_);
        }

        print $separator;

      } else {
        # Normal mode, list revisions of available merges
        print ' ', join (' ', @revs), "\n";
      }

    } else {
      w_report 'No merge available from ', $source->path_peg, ', abort.';
      return;
    }

    # If more than one merge available, prompt user to enter a revision number
    # to merge from, default to $revs [0]
    # --------------------------------------------------------------------------
    my $reply = ($non_interactive or @revs == 1) ? $revs[0] : &main::get_input (
      TITLE   => 'fcm merge',
      MESSAGE => 'Please enter the revision you wish to merge from',
      DEFAULT => $revs [0],
    );

    if (not defined ($reply)) {
      w_report 'Merge aborted by user.';
      return;
    }

    # Expand revision keyword if necessary
    if ($reply) {
      $reply = expand_rev_keyword (REV => $reply, URL => $target->project_url);
    }

    # Check that the reply is a number in the available merges list
    e_report $reply, ': not a revision in the list of available merges.'
      unless (grep {$_ == $reply} @revs);

    $source->pegrev ($1) if ($reply =~ /^(\d+)/);

    # If the working copy top is pointing to a sub-directory of a branch,
    # we need to check whether the merge will result in losing changes made in
    # other sub-directories of the source.
    if ($subdir and not $target->allow_subdir_merge_from ($source, $subdir)) {
      e_report 'SOURCE contains changes outside the current sub-directory.', "\n",
               'Please use a full tree for the merge, abort.';
    }

    # Calculate the base of the merge
    my $base = $target->base_of_merge_from ($source);

    # $source and $base must take into account the sub-directory
    my $s = Fcm::CmBranch->new (URL => $source->url_peg);
    my $b = Fcm::CmBranch->new (URL => $base->url_peg);

    $s->subdir ($subdir) if $subdir;
    $b->subdir ($subdir) if $subdir;

    # Diagnostic
    print 'About to merge in changes from ', $s->path_peg, ' compared with ',
          $b->path_peg, "\n";

    # Delta of the "svn merge" command
    @delta = ($b->url_peg, $s->url_peg);

    # Template message
    $mesg = 'Merged into ' . $target->path . ': ' . $source->path_peg .
            ' cf. ' . $base->path_peg . "\n";
  }

  # Run "svn merge" in "--dry-run" mode to see the result
  # ----------------------------------------------------------------------------
  my @out   = &run_command (
    [qw/svn merge --dry-run/, @delta],
    METHOD => 'qx', PRINT => ($dry_run and $verbose),
  );

  # Abort merge if it will result in no change
  if (not @out) {
    print 'This merge will not result in any change, abort.', "\n";
    return;
  }

  # Report result of "svn merge --dry-run"
  if (not $non_interactive) {
    print 'This merge will result in the following change',
          (@out > 1 ? 's' : ''), ':', "\n";
    print $separator, @out, $separator;
  }

  return if $dry_run;

  # Prompt the user to see if (s)he would like to go ahead
  # ----------------------------------------------------------------------------
  my $reply = $non_interactive ? 'y' : &main::get_input (
    TITLE   => 'fcm merge',
    MESSAGE => 'Would you like to go ahead with the merge?',
    TYPE    => 'yn',
    DEFAULT => 'n',
  );

  # Go ahead with merge only if user replies "y"
  if ($reply eq "y") {
    print "Performing merge ...\n";
    &run_command ([qw/svn merge/, @delta], PRINT => $verbose);

  } else {
    w_report 'Merge aborted by user.';
    return;
  }

  # Prepare the commit log
  # ----------------------------------------------------------------------------
  # Read in any existing message
  my $ci_mesg = Fcm::CmCommitMessage->new;
  $ci_mesg->read_file;
  $ci_mesg->auto_mesg ($mesg, ($ci_mesg->auto_mesg));
  $ci_mesg->write_file;

  if ($verbose) {
    print <<EOF;
${separator}The following line has been added to your commit message file:
$mesg
EOF
  }

  return;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &Fcm::Cm::cm_mkpatch ();
#
# DESCRIPTION
#   This is a FCM command to create a patching script from particular revisions
#   of a URL.
# ------------------------------------------------------------------------------

sub cm_mkpatch {
  my $usage = <<EOF;
mkpatch: Create patches from specified revisions of a URL
usage: fcm mkpatch [OPTIONS] URL [OUTDIR]

  URL must be the URL of a branch in a FCM project. If the URL is a
  sub-directory of a branch, it will use the root of the branch.

  Create patches from specified revisions of the specified URL. If OUTDIR is
  specified, the output is sent to OUTDIR. Otherwise, the output will be sent
  to a default location in the current directory (\$PWD/fcm-mkpatch-out). The
  output directory will contain the patch for each revision as well as a script
  for importing the patch.

  If a revision is specified with the --revision option, it will attempt to
  create a patch based on the changes at that revision. If a revision is not
  specified, it will attempt to create a patch based on the changes at the HEAD
  revision. If a revision range is specified, it will attempt to create a patch
  for each revision in that range (including the change in the lower range)
  where changes have taken place in the URL. No output will be written if there
  is no change in the given revision (range).

  The --exclude option can be used to exclude a path in the URL. The specified
  path must be a relative path of the URL. Glob patterns such as * and ? are
  acceptable. Changes in an excluded path will not be considered in the patch.
  A changeset containing changes only in the excluded path will not be
  considered at all.

  The --organisation option can be used to specify the name of your
  organisation. The command will attempt to parse the commit log message for
  each revision in the patch. It will remove all merge templates, replace links
  to Trac tickets with a simple string, and add information about the original
  changeset. If you specify the name of your organisation, it will replace Trac
  ticket links such as "ticket:123" to "Original \$organisation ticket 123",
  and report the orginal changeset with a message such as "Original
  \$organisation changeset 1000". Otherwise, it will report without the
  organisation name, e.g. "Original ticket 123" and "Original  changeset 1000".

Valid options:
  --exclude       arg : Exclude a path in the URL. Multiple paths can be
                        specified by using a colon-separated list of paths, or
                        by specifying this option multiple times.
  --organisation  arg : Specify the name of your organisation.
  -r [--revision] arg : Specify a revision number or a revision number range.
EOF

  # Print usage message if requested
  if (defined ($ARGV[0]) and grep {$_ eq $ARGV[0]} qw(--help -h)) {
    print $usage;
    return 1;
  }

  # Process command line options and arguments
  # ----------------------------------------------------------------------------
  my (@exclude, $organisation, $revision);
  GetOptions (
    'exclude=s'      => \@exclude,
    'organisation=s' => \$organisation,
    'r|revision=s'   => \$revision,
  );

  # Excluded paths, convert glob into regular patterns
  @exclude = split (/:/, join (':', @exclude));
  for (@exclude) {
    s#\*#[^/]*#; # match any number of non-slash character
    s#\?#[^/]#;  # match a non-slash character
    s#/*$##;     # remove trailing slash
  }

  # Organisation prefix
  $organisation = $organisation ? $organisation : 'original';

  # Make sure revision option is set correctly
  my @revs = $revision ? split (/:/, $revision) : ();
  @revs    = @revs [0, 1] if @revs > 2;

  # Arguments
  my ($u, $outdir) = @ARGV;

  if (not $u) {
    print $usage;
    return 1;
  }

  my $url = Fcm::CmUrl->new (URL => $u);
  e_report $u, ': URL is not a URL, abort.' if not $url->is_url;
  e_report $u, ': URL does not exist, abort.' if not $url->url_exists;
  e_report $u, ': URL is not a valid branch in a FCM project, abort.'
    if not $url->branch;

  $url->subdir ('');

  if (@revs) {
    # If HEAD revision is given, convert it into a number
    # --------------------------------------------------------------------------
    for my $rev (@revs) {
      $rev = $url->svninfo (FLAG => 'Revision') if uc ($rev) eq 'HEAD';
    }

  } else {
    # If no revision is given, use the HEAD
    # --------------------------------------------------------------------------
    $revs[0] = $url->svninfo (FLAG => 'Revision');
  }

  $revs[1] = $revs[0] if @revs == 1;

  # Check that output directory is set
  # ----------------------------------------------------------------------------
  $outdir = File::Spec->catfile (cwd (), 'fcm-mkpatch-out') if not $outdir;

  if (-e $outdir) {
    # Ask user to confirm removal of old output directory if it exists
    my $reply = &main::get_input (
      TITLE   => 'fcm mkpatch',
      MESSAGE => 'Output location ' . $outdir . ' exists. OK to overwrite?',
      TYPE    => 'yn',
      DEFAULT => 'n',
    );

    if ($reply ne 'y') {
      w_report 'fcm mkpatch: command aborted by user.';
      return 1;
    }

    rmtree $outdir or die $outdir, ': cannot remove';
  }

  # (Re-)create output directory
  mkpath $outdir or die $outdir, ': cannot create';
  print 'Output directory: ', $outdir, "\n";

  # Get and process log of URL
  # ----------------------------------------------------------------------------
  my @script     = (); # output script, from the log
  my %log        = $url->svnlog (REV => \@revs);
  my $url_path   = $url->path;
  my $file_count = 0;

  for my $rev (sort {$a <=> $b} keys %log) {
    # Look at the changed paths for each revision
    my @paths;

    # Skip excluded paths if necessary
    PATH: for my $path (sort keys %{ $log{$rev}{paths} }) {
      for my $exclude (@exclude) {
        (my $file = $path) =~ s#^$url_path/*##;

        next PATH if $file =~ m#^$exclude(?:/*|$)#;
      }

      push @paths, $path;
    }

    next unless @paths;

    # Parse commit log message
    my @msg = split /\n/, $log{$rev}{msg};
    for (@msg) {
      # Re-instate line break
      $_ .= "\n";

      # Remove line if it matches a merge template
      $_ = '' if /^Reversed r\d+(?::\d+)? of \S+$/;
      $_ = '' if /^Custom merge into \S+:.+$/;
      $_ = '' if /^Merged into \S+: \S+ cf\. \S+$/;

      # Modify Trac ticket link
      s/(?:#|ticket:)(\d+)/[$organisation ticket $1]/g;

      # Modify Trac changeset link
      s/(?:r|changeset:)(\d+)/[$organisation changeset $1]/g;
      s/\[(\d+)\]/[$organisation changeset $1]/g;
    }

    push @msg, '[' . $organisation . ' changeset ' . $rev . ']' . "\n";

    # Write commit log message in a file
    my $f_revlog = File::Spec->catfile ($outdir, $rev . '-log');
    open FILE, '>', $f_revlog or die $f_revlog, ': cannot open (', $!, ')';
    print FILE @msg;
    close FILE or die $f_revlog, ': cannot close (', $!, ')';

    # Create a directory for this revision in the output directory
    my $outdir_rev = File::Spec->catfile ($outdir, $rev);
    mkpath $outdir_rev or die $outdir_rev, ': cannot create';

    # Handle modified/copy/new path, export the path, + script to copy/add it
    for my $path (@paths) {
      next unless $log{$rev}{paths}{$path}{action} =~ /^[AMR]$/;

      (my $file = $path) =~ s#^$url_path/*##;

      # Download the file using "svn export"
      my $patch    = File::Spec->catfile ($outdir_rev, $file_count++);
      my $url_file = $url->url . '/' . $file . '@' . $rev;
      &run_command ([qw/svn export -q -r/, $rev, $url_file, $patch]);

      (my $patch_path = $patch) =~ s#^$outdir/*##;

      # Script to copy the file, if required
      my $is_newfile = 0;
      if ($log{$rev}{paths}{$path}{action} eq 'A') {
        if (exists $log{$rev}{paths}{$path}{'copyfrom-path'}) {
          # History exists for this file
          my $copyfrom_path = $log{$rev}{paths}{$path}{'copyfrom-path'};
          my $copyfrom_rev  = $log{$rev}{paths}{$path}{'copyfrom-rev'};

          # Check whether file is copied from a file under the specified URL
          # It is likely to be a new file if it is copied from outside of the
          # specified URL.
          $is_newfile = not ($copyfrom_path =~ s#^$url_path/*##);

          if ($is_newfile) {
            # File copied from outside of the specified URL
            # If it is copied from a branch, follow its history, stop on copy
            my $cp_url = Fcm::CmUrl->new (
              URL => $url->root . $copyfrom_path . '@' . $copyfrom_rev,
            );

            # Log of the copied file
            my %cp_log = $cp_url->svnlog (STOP_ON_COPY => 1);

            # "First" revision of the copied file
            my $cp_rev = (sort {$a <=> $b} keys %cp_log) [0];
            my %attrib = exists $cp_log{$cp_rev}{paths}{$cp_url->path}
                         ? %{ $cp_log{$cp_rev}{paths}{$cp_url->path} } : ();

            # Check whether the "first" revision is copied from elsewhere.
            if (exists $attrib{'copyfrom-path'}) {
              # Check whether source exists in the current branch
              my $cp_cp_url = Fcm::CmUrl->new (
                URL => $url->root . $attrib{'copyfrom-path'} . '@' .
                       $attrib{'copyfrom-rev'},
              );

              $cp_cp_url->branch ($url->branch);

              # If source exists in current branch, set up copy from the source
              if ($cp_cp_url->url_exists ($rev - 1)) {
                $is_newfile     = 0;
                (my $cp_cp_path = $cp_cp_url->path) =~ s#^$url_path/*##;

                push @script, 'svn copy ' . $cp_cp_path .  ' ' . $file;
              }
            }

          } else {
            # File copied from a location under the specified URL
            # Script to copy file
            push @script, 'svn copy ' . $copyfrom_path .  ' ' . $file;
          }

        } else {
          # History does not exist, must be a new file
          $is_newfile = 1;
        }
      }

      # Copy the "patch" into the file
      push @script, 'cp -r ${fcm_patch_dir}/' . $patch_path . ' ' . $file;

      # Script to add the file, if required
      push @script, 'svn add ' . $file
        if $log{$rev}{paths}{$path}{action} eq 'A' and $is_newfile;
    }

    # Handle deleted path, script to delete it
    for my $path (@paths) {
      next unless $log{$rev}{paths}{$path}{action} eq 'D';

      (my $file = $path) =~ s#^$url_path/*##;

      push @script, 'svn delete ' . $file;
    }

    # Script to commit the change
    push @script, 'svn commit -F ${fcm_patch_dir}/' . $rev . '-log';
    push @script, '';
  }

  # Write the script if necessary. Otherwise remove output directory
  # ----------------------------------------------------------------------------
  if (@script) {
    # Add line break to each line in @script
    @script = map {($_ ? $_ . ' || exit 1' . "\n" : "\n")} @script;

    # Write script to output
    my $out = File::Spec->catfile ($outdir, 'fcm-import-patch');
    open FILE, '>', $out or die $out, ': cannot open (', $!, ')';

    # Script header
    print FILE <<EOF;
#!/bin/sh
# ------------------------------------------------------------------------------
# NAME
#   fcm-import-patch
#
# SYNOPSIS
#   fcm-import-patch TARGET
#
# DESCRIPTION
#   This script is generated automatically by the "fcm mkpatch" command,
#   together with the revision "patches" it creates. The script imports the
#   patches into TARGET, which must either be a URL or a working copy of a
#   valid project tree that can accept the import of the patches.
#
#   Patch created from $organisation URL: $u
# ------------------------------------------------------------------------------

this=`basename \$0`

# Check argument
target=\$1

# First argument must be a URL or working copy
if [[ -z \$target ]]; then
  echo "\$this: the first argument must be a URL or a working copy, abort." >&2
  exit 1
fi

if [[ \$target == svn://*  || \$target == svn+ssh://* || \\
      \$target == http://* || \$target == https://*   || \\
      \$target == file://* ]]; then
  # A URL, checkout a working copy in a temporary location
  fcm_tmp_dir=`mktemp -d \$TMPDIR/\$0.XXXXXX`
  fcm_working_copy=\$fcm_tmp_dir
  svn checkout -q \$target \$fcm_working_copy || exit 1

else
  # A working copy, check that it does not have local changes
  status=`svn status \$target`

  if [[ -n \$status ]]; then
    echo "\$target: working copy contains changes, abort." >&2
    exit 1
  fi

  fcm_working_copy=\$target
fi

# Location of the patches, base on the location of this script
cd `dirname \$0` || exit 1
fcm_patch_dir=\$PWD

# Change directory to the working copy
cd \$fcm_working_copy || exit 1

# Commands to apply patches
EOF

    # Script content
    print FILE @script;

    # Script footer
    print FILE <<EOF;
# Remove temporary working copy, if necessary
if [[ -d \$fcm_tmp_dir && -w \$fcm_tmp_dir ]]; then
  rm -rf \$fcm_tmp_dir
fi

echo "\$this: finished normally."
#EOF
EOF

    close FILE or die $out, ': cannot close (', $!, ')';

    # Add executable permission
    chmod 0755, $out;

    # Diagnostic
    print $outdir, ': patch generated.', "\n";

  } else {
    # Remove output directory
    rmtree $outdir or die $outdir, ': cannot remove';

    # Diagnostic
    w_report 'No patch is required, abort.';
  }

  return 1;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &Fcm::Cm::cm_svn ();
#
# DESCRIPTION
#   This is a generic wrapper for all "other" Subversion commands.
# ------------------------------------------------------------------------------

sub cm_svn {
  &run_command (
    ['svn', @_, @ARGV],
    PRINT => ($_[0] ne 'cat' and not grep {$_ eq '--xml'} @ARGV),
    METHOD => 'exec',
  );
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &Fcm::Cm::cm_switch ();
#
# DESCRIPTION
#   This is a wrapper for the Subversion "switch" command.
# ------------------------------------------------------------------------------

sub cm_switch {
  if (grep {$_ eq '-h' or $_ eq '--help'} @ARGV or not @ARGV) {
    # Print usage message if requested
    print <<EOF;
usage: 1. switch URL [PATH]
       2. switch --relocate FROM TO [PATH...]

Note: if --relocate is not specified, "fcm switch" will only support the
      options --non-interactive, -r [--revision] and -q [--quiet].

EOF

    &run_command ([qw/svn switch --help/], PRINT => 1, METHOD => 'exec');

  } elsif (grep {$_ eq '--relocate'} @ARGV) {
    # If "--relocate" is specified, call the command "as is"
    cm_svn ('switch');
  }

  # "--help" and "--relocate" not specified, implement custom switch command

  # Get command line options
  my ($non_interactive, $rev, $quiet);
  GetOptions (
    'non-interactive' => \$non_interactive,
    'revision|r=s'    => \$rev,
    'quiet|q'         => \$quiet,
  );

  # The remaining arguments
  $rev = 'HEAD' if not $rev;

  # The remaining arguments
  my ($newurl_arg, $path) = @ARGV;

  # Make sure we are in a working copy
  if ($path) {
    e_report $path, ': does not exist, abort.' if not -e $path;

  } else {
    $path = cwd ();
  }

  e_report $path, ': not a working copy, abort.' if not &is_wc ($path);

  # Make sure we are at the top level of the working copy
  my $dir = &get_wct ($path);

  # Check for merge template in the commit log file in the working copy
  my $ci_mesg = Fcm::CmCommitMessage->new (DIR => $dir);
  $ci_mesg->read_file;
  e_report (
    (($path eq $dir) ? $ci_mesg->base : $ci_mesg->file),
    ': merge template exists, please remove it before running switch, abort.',
  ) if $ci_mesg->auto_mesg;

  # Check for any local modifications
  return if ! $non_interactive and &_abort_modified_wc ('fcm switch', $dir);

  # Get current URL information associated with the working copy
  my $oldurl = Fcm::CmBranch->new (URL => &get_url_of_wc ($dir));

  # Analyse new URL
  my $newurl = &_construct_branch_url ($newurl_arg, $oldurl);

  # Construct the switch command
  my @command = (
    qw/svn switch/,
    ($non_interactive ? '--non-interactive' : ()),
    ($rev             ? ('-r', $rev)        : ()),
    ($quiet           ? '--quiet'           : ()),
    $newurl->url,
    ($dir eq cwd () ? () : $dir),
  );

  # Execute the command
  &run_command (\@command, METHOD => 'exec', PRINT => 1);
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   $source = &_construct_branch_url ($src_url, $target);
#
# DESCRIPTION
#   The function takes a string $src_url, which is normally the SOURCE URL
#   argument for "merge" and "switch", and a target, which is an instance of a
#   Fcm::CmBranch object with a valid URL of a standard FCM branch. It returns
#   an instance of a Fcm::CmBranch object that represents a valid URL for
#   $src_url.
# ------------------------------------------------------------------------------

sub _construct_branch_url {
  my ($src_url, $target) = @_;

  my $source = Fcm::CmBranch->new (URL => $src_url);

  if (not $source->is_url) {
    # Not a full URL, construct full URL based on current URL
    $source->url_peg ($target->url_peg);

    my $path    = '';
    my $project = $target->project;

    # Construct the branch URL
    if ($src_url =~ m#^/*$project/(?:trunk|branches|tags)$#) {
      # Argument contains the full path under the repository root
      $path = $src_url;

    } elsif ($src_url =~ m#^/*trunk/*(?:@\d+)?$# or
             $src_url =~ m#^/*(?:trunk|branches|tags)/+#) {
      # Argument contains the full branch name
      $src_url =~ s#^/*##;
      $path    = $target->project_path . '/' . $src_url;

    } else {
      # Argument contains the shorter branch name
      $src_url =~ s#^/*##;
      $path    = $target->project_path . '/branches/' . $src_url;
    }

    $source->path_peg ($path);
  }

  # Replace source sub-directory with the target sub-directory
  $source->subdir ($target->subdir);

  # Ensure that the branch name exists
  e_report $src_url, ': not a valid URL, abort.'
    if not $source->url_exists;

  # Ensure that the branch name is valid
  e_report $src_url, ': not a standard branch in a FCM project, abort.'
    if not $source->branch;

  # Ensure that the source and target URLs are in the same project
  e_report 'Source and target URLs are in different projects, abort.'
    if $source->project_url ne $target->project_url;

  return $source;
}

# ------------------------------------------------------------------------------
# SYNOPSIS
#   &_abort_modified_wc ($title, [$wc]);
#
# DESCRIPTION
#   The function checks for any local modifications in a working copy and
#   prompts the user whether he/she wants to continue with the command. $title
#   is the title of the current command. If $wc is specified, it must be the
#   path to a working copy. Otherwise, the current working directory is used.
# ------------------------------------------------------------------------------

sub _abort_modified_wc {
  my ($title, $wc) = @_;

  my @status = &run_command ([qw/svn status/, ($wc ? $wc : ())], METHOD => 'qx');

  if (@status) {
    print 'You have local modifications:', "\n", @status;
    my $reply = &main::get_input (
      TITLE   => $title,
      MESSAGE => 'Are you sure you want to continue?',
      TYPE    => 'yn',
      DEFAULT => 'n',
    );

    # Abort if user gives any reply other than "y"
    if ($reply ne 'y') {
      w_report $title, ': command aborted by user.';
      return 1;
    }
  }
}

# ------------------------------------------------------------------------------

1;

__END__
