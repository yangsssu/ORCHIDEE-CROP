# codedox.awk - filter of ORCHIDEE f90 files
# command : gawk -f codedox.awk [-d] file.f90
# create a file.f90_preproc_codedox filtered

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
      if ( ldebug ) { 
	  print "-> " str  | "cat 1>&2"
      } else {
	  print str  | "cat 1>&2"
      }
  }
}
function myprint(str) {
    if ( ldebug ) { 
	print str  | "cat 1>&2"
    } else {
	print str >> output_file
    }
}


function flush_line() {
    if ( nblc > 0 ) {
	debugprint("==> flush")
	for ( i=0; i < nblc; i++ ) {
	      myprint(line[i]) 	    
	}
	nblc=0
	iblc=0
    }
}

function code_intent(iblc) {
    debugprint("line with intent")
    if (match(line[iblc], "intent *\\(in\\)")) {
	sub(/.*\!+>*\-*/, " !> [in]",line[iblc])
    } else if (match(line[iblc], "intent *\\(out\\)")) {
	sub(/.*\!+>*\-*/, " !> [out]",line[iblc])
    } else if (match(line[iblc], "intent *\\(inout\\)")) {
	sub(/.*\!+>*\-*/, " !> [in,out]",line[iblc])
    } else {        
        print "unknown intent error : " line[iblc]
	debugprint("unknown intent error : " line[iblc])
	exit 1
    } 
    if ( icont == 1 ) {
	line[iblc]=line[iblc]  "\\n"
    }
    debugprint("line after Doxygen intent :" line[iblc])

    sub(/\!.*/, "")
    line[nblc]=$0
    nblc++
    debugprint("Only code intent :" $0)
}

function code_INTENT_ (iblc) {
    debugprint("line with INTENT")
    if (match(line[iblc], "INTENT *\\(in\\)")) {
	sub(/.*\!+>*\-*/, " !> [in]",line[iblc])
    } else if (match(line[iblc], "INTENT *\\(out\\)")) {
	sub(/.*\!+>*\-*/, " !> [out]",line[iblc])
    } else if (match(line[iblc], "INTENT *\\(inout\\)")) {
	sub(/.*\!+>*\-*/, " !> [in,out]",line[iblc])
    } else if (match(line[iblc], "INTENT *\\(IN\\)")) {
	sub(/.*\!+>*\-*/, " !> [in]",line[iblc])
    } else if (match(line[iblc], "INTENT *\\(OUT\\)")) {
	sub(/.*\!+>*\-*/, " !> [out]",line[iblc])
    } else if (match(line[iblc], "INTENT *\\(INOUT\\)")) {
	sub(/.*\!+>*\-*/, " !> [in,out]",line[iblc])
    } else {	
        print "unknown INTENT error : " line[iblc]
	debugprint("unknown INTENT error : " line[iblc])
	exit 1
    } 
    if ( icont == 1 ) {
	line[iblc]=line[iblc]  "\\n"
    }
    debugprint("line after Doxygen INTENT :" line[iblc])

    sub(/\!.*/, "")
    line[nblc]=$0
    nblc++
    debugprint("Only code intent :" $0)
}

#==========================
BEGIN {
    ldebug=0
    debug=0
    if (ARGV[1] == "-d") {
	debug=1
	file=ARGV[2]
 	delete ARGV[1] 
    } else if (ARGV[1] == "-dd") {
	debug=1
	ldebug=1
	file=ARGV[2]
	delete ARGV[1] 
    } else {
	file=ARGV[1]
    }
    debugprint("traitement de " file)

    output_file=file "_preproc_codedox"
    if (debug) {
	if ( ! ldebug ) {
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
    }

# When exit statement, 'END' rule is always executed, so defined a exit_value to manage this
    exit_value=0
    if ( ARGV[1] == "-h" ) {
	debugprint("Usage: codedox.awk [-h] [-d|-dd] file.f90")
	debugprint("")
	debugprint("Args:")
	debugprint("      file.f90 = source code to be parsed")
	debugprint("") 
	debugprint("Options:" )
	debugprint("       -h = this usage")
	debugprint("       -d = debug mode")
	debugprint("       -dd = debug mode stdout")
	debugprint("") 
	exit_value=1
	exit
    }
    # Number of previous lines commented
    nblc=0

    # Flag to point indented declaration
    idecl=0
    # index of first declaration comment 
    icdecl=0
    # Flag to point a continuation line inside a declaration
    icont=0
    # Number of the first line of declaration group inside line tab
    nicont=0
}

#==========================
{
  debugprint($0)

  # Divers modification de la ligne pour la compatibilité Doxygen
  if (match($0, "!+")) {
      
      if (match($0, "!\\?")) {
	  debugprint("Wrong comment")
	  sub(/\!\?/, "!")
	  debugprint("Correct wrong comment" $0)
      } else if (match($0, "f90doc")) {
	  debugprint("suppression de la ligne")
	  next
      } else if (match($0, "@call")) {
	  debugprint("line with @call")
	  sub(/@call/, "@see",$0)
	  debugprint("line after correct call :" $0)
      } else if (match($0, "@Version")) {
          debugprint("line with @Version")
          sub(/@Version/, "@version")
          debugprint("line after Doxygen @version :" $0)
      } else if (match($0, "@tex")) {
	  match($0,"@tex([^@]+)@endtex",math)
          debugprint("line with formula :" math[1])
	  $0 = substr($0,1,match($0,"@tex")-1) "@htmlonly" math[1] "@endhtmlonly @latexonly" math[1] "@endlatexonly" substr($0,match($0,"@endtex")+7)
      }

      # traitement des simples cotes
      if (match($0, "'") && ! match($0, "'.*'") ) {
          debugprint("line with single cote")
          sub(/'/, "''",$0)
          debugprint("line without single cote : " $0)
      }
  }

  if (match($0, "!+")) {
      debugprint("line with comment")
      
      if ( idecl == 1 ) {
	  debugprint("inside a decl " (match($0,"^[[:space:]]+!")) " " match($0,"!") " " icdecl ".")
	  if ( (match($0,"^[[:space:]]+!")) && (match($0,"!") == icdecl) ) { 	
	      line[nblc]=line[nblc-1]
	      sub(/^[[:space:]]+\!+>*/," !>")
	      # on écrit le nouveau commentaire avant le précédant
	      line[nblc-1]=$0 "\\n"
	      nblc++
	      debugprint("continuation of declaration")
	      next
	  } else if ( icont == 1 ) {
	      if (match($0,"!")) { 
		  debugprint("continuation of declaration with &" nicont) 
		  for ( i=nblc; i > nicont; i-- ) {
		      line[i]=line[i-1]
		  }
		  nblc++

		  line[nblc]=substr($0,1,icdecl-1)
		  nblc++

		  line[nicont+1]=line[nicont]

		  line[nicont]=substr($0,icdecl)
		  sub(/^\!+>*/," !>",line[nicont])
		  line[nicont]=line[nicont] "\\n"
		  for ( i=nicont-1; i < nblc; i++ ) {
		      debugprint(":" i line[i])
		  }
	      } else {
		  line[nblc]=$0
		  nblc++
		  debugprint("no comment in continuation of declaration with &") 
	      }
	      
	      if ( match(line[nblc-1], "&") ) {
		  if ( match(line[nblc-1], "&.*&") ) {
		      icont = 1
		      nicont=nicont+1
		      debugprint("next line with &") 
		  } else if ( match(line[nblc-1], "^[[:space:]]*&") ) {
		      icont = 0
		      nicont = 0
		      debugprint("next line without &") 
		  } else {
		      icont = 1
		      nicont=nicont+1
		      debugprint("next line with &") 
		  }
	      } else {
		  icont = 0
		  nicont = 0
		  debugprint(match(line[nblc-1], "^[[:space:]]+&*[^!]+&") match(line[nblc-1], "^[[:space:]]+[^!]+&[[:space:]]*!*"))
	      }
	      next
	  } else {
	      idecl=0
	      icont = 0
	      nicont = 0
	      flush_line()
	  }
      }

      iblc=nblc
      line[nblc]=$0
      nblc++

      # commentary after a parameter documentation : need flush previous lines without Doxygen doc
      if (match(line[iblc], "^[^!]+intent *\\(.*\\).*::")) {
	  idecl=1
	  icdecl=match($0,"!")
	  if ( match(line[iblc], "[^!]*::[^!]*&") ) {
	      icont = 1
	      nicont=nblc
	      debugprint("next line with &") 
	  }
	  code_intent(iblc)

      } else if (match(line[iblc], "^[^!]+INTENT *\\(.*\\).*::")) {
	  idecl=1
	  icdecl=match($0,"!")
	  if ( match(line[iblc], "[^!]*::[^!]*&") ) {
	      icont = 1
	      nicont=nblc
	      debugprint("next line with &") 
	  }
	  code_INTENT_(iblc)

      } else if (match(line[iblc], "^[^!]+::")) {
	  debugprint("line with declaration")

	  idecl=1
	  icdecl=match($0,"!")
	  if ( match(line[iblc], "[^!]*::[^!]*&") ) {
	      icont = 1
	      nicont=nblc
	      debugprint("next line with &") 
	  }

	  sub(/.*\!+>*/, " !>",line[iblc])
	  debugprint("line after Doxygen declaration :" line[iblc])

	  sub(/\!.*/, "")
	  debugprint("Only code declaration :" $0)
	  line[nblc]=$0
	  nblc++

      }
      next
  } else if (match($0, "^[^!]*subroutine.*\\(.*") || match($0, "^[^!]*SUBROUTINE.*\\(.*") || 
	     ( match($0, "^[^!]*subroutine ") && ! (match($0, "^[[:space:]]+end ")) ) || 
	     ( match($0, "^[^!]*SUBROUTINE ") && ! (match($0, "^[[:space:]]+END ")) ) || 
	     match($0, "^[^!]*function.*\\(.*") || match($0, "^[^!]*FUNCTION.*\\(.*") || 
	     match($0, "^[^!]*module *") || match($0, "^[^!]*MODULE *") || 
	     match($0, "^[^!]*interface *") || match($0, "^[^!]*INTERFACE *") ||
	     match($0, "^[^!]+::")) {

      debugprint("f90 declaration")

      if ( idecl == 1 ) {
	  idecl=0
	  icont=0
	  flush_line()
      } 
      if (nblc > 0 && ( match($0, "^[^!]+intent *\\(.*\\).*::") ||
			match($0, "^[^!]+INTENT *\\(.*\\).*::") )) {
	  if (match($0, "\\(in\\)") || match($0, "\\(IN\\)")) {
	      sub(/.*\!+>*\-*/, " !> [in]",line[iblc])
	  } else if (match($0, "\\(out\\)") || match($0, "\\(OUT\\)")) {
	      sub(/.*\!+>*\-*/, " !> [out]",line[iblc])
	  } else if (match($0, "\\(inout\\)") || match($0, "\\(INOUT\\)")) {
	      sub(/.*\!+>*\-*/, " !> [in,out]",line[iblc])
	  } else {	
	      print "unknown INTENT error : " $0
	      debugprint("unknown INTENT error : " $0)
	      exit 1
	  }
      } else {

	  for ( i=0; i < nblc; i++ ) {
	      #  : ajout des commentaires Doxygen
	      if (match(line[i], "!<")) {
		  sub(/\^ *\!+</, " !>",line[i])
	      } else {
		  sub(/^ *\!\!/, " !>",line[i])
	      }

	      debugprint("traitement des lignes précédentes : " line[i])
	  }
      }
      flush_line()
      myprint($0)
      next
  } else {
      debugprint("normal line")

      if ( idecl == 1 ) idecl=0

      flush_line()
      myprint($0)
  }
}

END {
    if ( ! ldebug ) close(output_file)
}
