#!/usr/bin/awk -f 

# correct-cvs-diff.awk try to correct files PATH in diff lines inside wrong 
# CVS diff files for use with patch.
# CVS diff give header like :
#
# Index: src/myfile.f90
# ===================================================================
# RCS file: ...DIR/src/myfile.f90,v
# retrieving revision 1.8
# diff -U 2 -r1.8 myfile.f90
# --- src/myfile.f90	28 Jan 2009 08:32:45 -0000	1.8
# +++ src/myfile.f90	7 Oct 2010 16:04:00 -0000
# 
# But the diff line is wrong because the path for myfile is incomplete.
# The patch will ask for the true path :
# 
# can't find file to patch at input line 23
# Perhaps you should have used the -p or --strip option?
# The text leading up to this was:
# --------------------------
# |Index: src/myfile.f90
# |===================================================================
# |RCS file: ...DIR/src/myfile.f90,v
# |retrieving revision 1.8
# |diff -r1.8 myfile.f90
# --------------------------
# File to patch:
#
# This script will correct the diff file by adding the minimal true PATH : 
# diff -U 2 -r1.8 src/myfile.f90
# 
# command :
# correct-cvs-diff.awk [-d] cvs-diff-file

#**************************************************************
# Author: Martial.Mancip
# Contact: Martial.Mancip_ipsl.jussieu.fr
# $Date: 
# $Author: $
# $Revision: $
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
# History:
# Modification:
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
        print "Usage: correct-cvs-diff.awk [-d] cvs-diff-file"
        exit_value=-1
        exit
  }
    
  index_found=0
  counter=0
  errcounter=0
  errorfile[1]=""
}

#==========================
{

  myprint($0) 

  if (index_found == 0 && match($0, "Index:.*")) {
    index_found=1
    counter=counter+1

    nb=split($0,index_line, " ")
    myprint("Index " counter " found with nb : " nb)
    for (elt in index_line) {
	myprint("index " elt "- elt  :" index_line[elt])
    }
    myprint("Index_path : " index_line[2])
    print $0
  }
  else if (index_found == 1 && match($0, "diff .*")) {
    myprint("diff found")
    index_found=0

    nb=split($0,diff_line, " ")
    myprint("nb  :" nb)
    for (elt in diff_line) {
	myprint("diff " elt "- elt  :" diff_line[elt])
    }
    myprint("diff_path : " diff_line[nb])

    diff_line[nb]=index_line[2]
    i=1
    while (i <= nb) {
	printf("%s ",diff_line[i])
	i=i+1
    }
    printf("\n")
  }
  else {
    print $0
  }
}
