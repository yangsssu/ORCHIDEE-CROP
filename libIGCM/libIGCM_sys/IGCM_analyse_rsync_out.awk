#!/usr/bin/awk -f 
# IGCM_analyse_rsync_out - filter rsync output in a job output :
# command :
# IGCM_analyse_rsync_out.awk [-d] job_output.out

#**************************************************************
# Author: Martial.Mancip
# Contact: Martial.Mancip__at__ipsl.jussieu.fr
# $Revision:: 373                                      $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2010-10-29 12:37:36 +0200 (Fri, 29 Oct 2010) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
#
#**************************************************************

#==========================
function myprint(str) {
  if (debug) {
     print str
  }
}


#==========================
BEGIN {
#  print "traitement de " ARGV[1]

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

  exit_value=0
  if (nbarg != 2) {
        print "Usage: analyse_rsync_out.awk [-d] file"
        exit_value=-1
        exit
  }
    
  rsync_found=0
  speedup_found=0
  counter=0
  thisfile[1]=""
  errcounter=0
  errorfile[1]=""
}

#==========================
{

  myprint($0) 

  if (rsync_found == 0 && match($0, ".*/rsync .*")) {
    speedup_found = 0
    rsync_found=1
    counter=counter+1

    nb=split($0,felts, " ")
    myprint("nb  :" nb)
    for (elt in felts) {
	myprint(elt "- elt  :" felts[elt])
    }
    myprint("elts_split  :" felts[4])

    thisfile[counter]=felts[4]
  }
  else if (rsync_found == 1 && match($0, ".* speedup is .*")) {
    myprint("speedup_found = 1")
    speedup_found = 1
  }
  else if (speedup_found == 1 && match($0, "rsync error:.*")) {
    errcounter=errcounter+1
    errorfile[errcounter]=thisfile[counter]
    myprint("=============================================")
    myprint("WARNING : Rsync ERROR for " ARGV[1])
    myprint("counter  :" counter)
    myprint("file  :" thisfile[counter])
    thisfile[counter]=""
    counter=counter-1
  } 
  else {
    if ( speedup_found == 1 ) {
      rsync_found=0
      speedup_found=0
    }
  }
}

#==========================
END {
  if ( exit_value != -1 ) {

    myprint("=============================================")
    myprint("Rsync for " ARGV[1])
    myprint("counter  :" counter)
    for (i=1; i<=counter; i++) {  
      myprint("file  :" thisfile[i])
    }
    if (errcounter > 0) {
      debug=1
      myprint("Error counter  :" errcounter)
      for (i=1; i<=errcounter; i++) {  
        myprint("Error file  :" errorfile[i])
      }
      exit_value=errcounter
      print exit_value
      exit exit_value
    } 
    exit_value=0
    exit 0
  }
}
