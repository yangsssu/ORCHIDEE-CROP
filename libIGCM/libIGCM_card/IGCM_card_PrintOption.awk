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
        delete ARGV[1] ; delete ARGV[3] ; delete ARGV[4]
	nbarg--
} else {
        debug=0
        file=ARGV[1]
        section=ARGV[2]
        option=ARGV[3]
        delete ARGV[2] ; delete ARGV[3]
}

# When exit statement, 'END' rule is always executed, so defined a exit_value to manage this
exit_value=0
if (nbarg != 4) {
	print "Usage: IGCM_card_PrintOption [-d] file section option" 
	print 
	print "Args:"
	print "      file = file at CARD format" 
	print "      section = section to find" 
	print "      option = option to find" 
	print 
	print "Options:" 
	print "       -d = debug mode" 
	print 
	exit_value=1
	exit
}

section_found=0
option_found=0

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

} else if (section_found == 1 && (match($0, "^[ ]*" option "[ ]*="))) {

	myprint("---->option found")
	option_found=1
	
	# Extract string after '='
	option_value=substr($0, index($0, "=")+1)

	# Remove space after '='
	gsub(/^[ ]*/, "", option_value)

	# If continuation character '\' get next lines
	if (match($0,/\\$/)) {
		tmp=$0
		tmp=substr(tmp, 1, length(tmp)-1)
		gsub(/[ \t]*/, "", tmp)
		option_value=tmp
		getline tmp
		while (match(tmp,/\\$/)) {
			tmp=substr(tmp, 1, length(tmp)-1)
			gsub(/[ \t]*/, "", tmp)
			option_value=option_value " " tmp
			getline tmp 
			}
		gsub(/[ \t]*/, "", tmp)
		option_value=option_value " " tmp
	}

	exit

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
myprint("option       ====> " option)
myprint("value        ====> " option_value)

if (section_found == 0) {
	print "Error: Section not found"
} else if (option_found == 0) {
	print "Error: Option not found"
} else {
	print option_value
}

}

}
 
#==========================
