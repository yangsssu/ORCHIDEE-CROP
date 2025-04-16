# codealgo.awk - filter of ORCHIDEE f90 files
# command : gawk -f codealgo.awk [-d] file.f90
# create a file.f90_preproc_codealgo filtered with algo comments inside each routine all group 
# in the Doxygen comment just before its beginning.  

#**************************************************************
# Author: Martial.Mancip
# Contact: Martial.Mancip__at__ipsl.jussieu.fr
# $Revision::                                          $ Revision of last commit
# $Author::                                            $ Author of last commit
# $Date:: 
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
# Modification: 
#
#**************************************************************
#==========================
function debugprint(str) {
  if (debug) {
     print str  | "cat 1>&2"
  }
}
function myprint(str) {
    print str >> output_file
}


function flush_line() {
    if ( nblc > 0 ) {
	if ( inside_algo == 1 ) {
	    myprint(" !> </ul>")
	}
	for ( i=1; i < nblc+1; i++ ) {
	      myprint(line[i]) 	    
	}
	nblc=0
    }
}


#==========================
BEGIN {

    nbarg=ARGC
    if (ARGV[1] == "-d") {
	debug=1
	file=ARGV[2]
	delete ARGV[1] 
	nbarg--
    } else {
	debug=0
	file=ARGV[1]
    }
    output_file=file "_preproc_codealgo"
    if (debug) {
	lscommand=("ls " output_file)
	outsys=system(lscommand)
	debugprint("output of ls : " outsys)
	if ( outsys == 0 ) {
	    debugprint("Already find " output_file)
	    outsys=system("rm " output_file)
	    debugprint("output of rm : " outsys)
	    system("touch " output_file)
	}
    }

# When exit statement, 'END' rule is always executed, so defined a exit_value to manage this
    exit_value=0
    if (nbarg != 2) {
	debugprint("Usage: codealgo.awk [-d] file.f90")
	debugprint("")
	debugprint("Args:")
	debugprint("      file.f90 = source code to be parsed")
	debugprint("") 
	debugprint("Options:" )
	debugprint("       -d = debug mode")
	debugprint("") 
	exit_value=1
	exit 2
    }
    debugprint("filter " file " in " output_file) 

    # Number of previous lines commented
    nblc=0
    # Flag indicates inside a routine
    routine = 0
    routine_name = ""
    # detect begin of algo liste
    inside_algo=0

    IGNORECASE=1
}

#==========================
{
  debugprint($0)

  if ( match($0, "^[^!('\"]*subroutine") || match($0, "^[^!('\"]*function") ) {

      debugprint("routine line.")

      nblc++
      line[nblc]=$0

      # Begin of algo search
      if ( match(line[nblc],"^[[:space:]]*end") ) {
	  debugprint("End of the routine.")
	  if ( routine == 0 ) {
	      # error !
	      debugprint("error ! End of routine has been detected but no routine was opened.")
	      exit 1
	  } else {
	      if ( match(line[nblc],routine_name) ) {
		  # here we flush the whole routine
		  debugprint("Normal End of a routine : flush previous lines.")
		  flush_line()
		  routine = 0
		  inside_algo = 0
		  nblc = 0
	      } else {
		  debugprint("Error in a routine : routine_name " routine_name " doesn't match.")
		  exit 3
	      }
	  }
      } else {
	  debugprint("Begining of a new routine.")

	  if ( routine == 1 ) {
	      if ( match(line[nblc],routine_name) ) {
		  debugprint("routine : " routine_name " detect two times. Precompilation ? OK.")
	      } else {
		  # error !
		  debugprint("error ! Already find a routine not closed.")
		  exit 1
	      }
	  } else {
	      routine_name=$2
	      sub(/\(.*/,"",routine_name)
	      debugprint("routine : " routine_name)
	      routine = 1
	  }
      }

  } else if ( routine == 1 ) {

      nblc++
      line[nblc]=$0

      if (match(line[nblc], "^[[:space:]]+\\!+>*[[:space:]]+[[:digit:]]+\\.*"))
      {
	  debugprint("New algo line.")
	  if ( inside_algo == 0 ) {
	      myprint(" !> ")
	      myprint(" !> ALGORITHM : ")
	      myprint(" !> <ul>")
	      debugprint("ALGORITHM.")
	      inside_algo = 1
	  }
	  sub(/\!+>*/,"!> <li> ")
	  myprint($0)
      }
  } else if ( routine == 0 ) {
	  myprint($0)
  }
}

END {
    close(output_file)
}
