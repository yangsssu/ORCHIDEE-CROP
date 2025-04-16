# codeinc.awk - filter of ORCHIDEE f90 files
# command : gawk -f codeinc.awk [-d] file.f90
# create a file.f90_preproc_codeinc filtered

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


#==========================
BEGIN {
    debugprint("traitement de " ARGV[1])

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
    output_file=file "_preproc_codeinc"
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
	debugprint("Usage: codeinc.awk [-d] file.f90")
	debugprint("")
	debugprint("Args:")
	debugprint("      file.f90 = source code to be parsed")
	debugprint("") 
	debugprint("Options:" )
	debugprint("       -d = debug mode")
	debugprint("") 
	exit_value=1
	exit
    }
    codeinc=0
    linecodeinc[1]=""
    line[1]=""
}

#==========================
{
  debugprint($0)

# Get information with MPI Program Information output on NEC
  if (match($0, " *!> @codeinc *")) 
  {
      codeinc=1
      nblines=0
      Length=(match($0,"!>")-1)
      spaces=substr($0,1,Length)
      debugprint("Find @codeinc !" spaces Length)
      myprint(spaces "!> @code")
      next
  } 
  else if ( codeinc == 1 )
  {
      if (match($0, " *!> @endcodeinc *")) 
      {
	  codeinc=0
	  debugprint("Find @endcodeinc !")
	  for (i=0; i<nblines; i++) {  
	      myprint(spaces "!> " linecodeinc[i])
	  }
 	  myprint(spaces "!> @endcode")
	  for (i=0; i<nblines; i++) {  
	      myprint(line[i])
	  }
	  nblines=0
      }
      else
      {
	  line[nblines]=$0
	  debugprint(nblines " | " line[nblines])
	  linecodeinc[nblines]=substr(line[nblines],Length+1)
	  debugprint(linecodeinc[nblines])
	  nblines++
      }
  }
  else
  {
      myprint($0)
  }

}

END {
    close(output_file)
}
