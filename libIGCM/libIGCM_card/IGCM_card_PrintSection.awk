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
        delete ARGV[1] ; delete ARGV[3]
	nbarg--
} else {
        debug=0
        file=ARGV[1]
        section=ARGV[2]
        delete ARGV[2]
}

# When exit statement, 'END' rule is always executed, so defined a exit_value to manage this
exit_value=0
if (nbarg != 3) {
	print "Usage: IGCM_card_PrintSection [-d] file section" 
	print 
	print "Args:"
	print "      file = file at CARD format" 
	print "      section = section to find" 
	print 
	print "Options:" 
	print "       -d = debug mode" 
	print 
	exit_value=1
	exit
}

section_found=0
option_list="null"

}

#==========================
{

myprint($0) 

# Do not consider commented lines (#...)
if (! match($0,/^#/)) {

# Find section with delimiters '[' and ']'
if (match($0, "\\[" section "\\]")) {

	myprint("---->section found")
	section_found=1

} else if (section_found == 1 && (match($0, "[.]*="))) {

	myprint("---->option found")

	# Extract string after '='
	option_value=substr($0, 1, index($0, "=")-1)

	# Remove space before '='
	gsub(/[ ]*/, "", option_value)

	# Add options
	# If 1st option
	if (match(option_list,"null")) {
		option_list=option_value
	} else {
		option_list=option_list " " option_value
	}

} else if (section_found == 1 && match($0, /^[\[*\]]/)) {

	myprint("---->end section")
	exit

}

}

}

#==========================
END {

if (! exit_value ) {

myprint("###############################")
myprint("section      ====> " section)

if (section_found == 0) {
	print "Error: Section not found"
} else if (match(option_list,"null")) {
	print "Error: Option not found"
} else {
	print option_list
}

}

}
 
#==========================
