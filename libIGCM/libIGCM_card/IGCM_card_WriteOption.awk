#**************************************************************
# Author: Patrick Brockmann
# Contact: Patrick.Brockmann__at__cea.fr
# $Revision:: 475                                      $ Revision of last commit
# $Author:: sdipsl                                     $ Author of last commit
# $Date:: 2011-05-31 15:12:20 +0200 (Tue, 31 May 2011) $ Date of last commit
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

nbarg=ARGC
if (ARGV[1] == "-d") {
        debug=1
        file=ARGV[2]
        section=ARGV[3]
        option=ARGV[4]
        value=ARGV[5]
        delete ARGV[1] ; delete ARGV[3] ; delete ARGV[4] ; delete ARGV[5]
	nbarg--
} else {
        debug=0
        file=ARGV[1]
        section=ARGV[2]
        option=ARGV[3]
        value=ARGV[4]
        delete ARGV[2] ; delete ARGV[3] ; delete ARGV[4]
}


# When exit statement, 'END' rule is always executed, so defined a exit_value to manage this
exit_value=0
if (nbarg != 5) {
	print "Usage: IGCM_card_WriteOption [-d] file section option value"
	print 
	print "Args:"
	print "      file = file at CARD format" 
	print "      section = section to find" 
	print "      option = option to find" 
	print "      value = value option to write" 
	print 
	print "Options:" 
	print "       -d = debug mode" 
	print
	exit_value=1
        exit
}

section_found=0
section_inside=0
option_found=0
}

#==========================
{

myprint($0) 

# Find section with delimiters '[' and ']'
if (match($0, "\\[" section "\\]")) {

	myprint("---->section found")
	section_found=1
	section_inside=1
	print $0

} else if (section_inside == 1 && (match($0, "^[ ]*" option "[ ]*="))) {

	myprint("---->option found")
	option_found=1

	print option "= " value
	myprint("---->new value ")

} else if (section_inside == 1 && match($0, /^[\[*\]]/)) {

	myprint("---->end section")
	section_inside=0
	print $0

} else {
	print $0
}

}

#==========================
END {

    if (! exit_value ) {

	myprint("###############################")
	myprint("section      ====> " section)
	myprint("option       ====> " option)
	myprint("value        ====> " value)
	
	if (section_found == 0) {
	    print "Error: Section not found"
	} else if (option_found == 0) {
	    print "Error: Option not found"
	}	
	
    } else {

	print "Error in with IGCM_WriteOption : ", section, option

    }

}
 
#==========================
