# IGCM_add_out - filter of output of the job :
# command :
# IGCM_add_out.awk job_output.out REALTIME USERTIME SYSTIME

#**************************************************************
# Author: Martial.Mancip
# Contact: Martial.Mancip__at__ipsl.jussieu.fr
# $Revision:: 671                                      $ Revision of last commit
# $Author:: mafoipsl                                   $ Author of last commit
# $Date:: 2012-05-21 08:24:42 +0200 (Mon, 21 May 2012) $ Date of last commit
# IPSL (2006)
#  This software is governed by the CeCILL licence see libIGCM/libIGCM_CeCILL.LIC
# Modification: Patrick.Brockmann@cea.fr
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

# When exit statement, 'END' rule is always executed, so defined a exit_value to manage this
  exit_value=0
  if (nbarg != 2) {
    print "Usage: IGCM_add_out.awk [-d] file.output" 
    print 
    print "Args:"
    print "      file.output = output file of at Job to be parsed" 
    print 
    print "Options:" 
    print "       -d = debug mode" 
    print 
    exit_value=1
    exit
  }

  Information_found=0
  Information_ksh_SX_found=0

  RealTime=0.
  UserTime=-1.
  SysTime=0.
  VectorTime=0.
  InstCount=0.
  VInstCount=0.
  VElementCount=0.
  FLOPCount=0.
  MOPS=0.
  MFLOPS=0.
  AVLength=0.
  VOpRatio=0.
  MemorySize=0.
  MIPS=0.
  ICache=0.
  OCache=0.
  Bank=0.

  counter=0

  RunDateBegin="2000-01-01T00:00:00"
  RunDateEnd="2000-01-01T00:00:00"

}

#==========================
{
  myprint($0) 

# Get information with MPI Program Information output on NEC
  if (match($0, "     \\*\\*\\*\\*\\*\\*  Program Information  \\*\\*\\*\\*\\*\\*")) 
  {
    Information_found=1
    counter=counter+1
#   print $0      
#    print counter
    next
  }
  else if (Information_found == 1 && (match($0, ".*:.*"))) 
  {
    where=(match($0,"[a-zA-Z]"))
    Length=(match($0, " *:"))-where
    whereDot=(match($0, ":"))
    info=substr($0,where,Length)
    whereNum=(match($0, "[0-9]"))
    Res=substr($0,whereNum)
#    print where "," Length "," whereDot "," whereNum " '" info "' :" Res 

    if( info=="Real Time (sec)" ) {
#      print "|" Res "|"
#      printf("RealTime = %10.5f\n",0.037600)
#      printf("RealTime = %10.5f\n",Res)
      if ( Res > RealTime ) {
        myprint( "NEC Real Time updated" )
	    RealTime=Res
      }
      next
    }
    	  
    if( info=="User Time (sec)" ) {
      myprint( "NEC User Time " )
      UserTime=UserTime+Res
      next
    }

    if( info=="Sys  Time (sec)" ) {
      SysTime=SysTime+Res
      next
    }

    if( info=="Vector Time (sec)" ) {
      VectorTime=VectorTime+Res
      next  
    }

    if( info=="Inst. Count" ) {
      InstCount=InstCount+Res
      next
    }

    if( info=="V. Element Count" ) {
      VInstCount=VInstCount+Res
      next  
    }

    if( info=="V. Element Count" ) {
      VElementCount=VElementCount+Res
      next
    }

    if( info=="FLOP Count" ) {
      FLOPCount=FLOPCount+Res
      next  
    }

    if( info=="MOPS" ) {
      MOPS=MOPS+Res
      next
    }

    if( info=="MFLOPS" ) {
      MFLOPS=MFLOPS+Res
      next  
    }

    if( info=="A.V. Length" ) {
      AVLength=AVLength+Res
      next
    }

    if( info=="V. Op. Ratio (%)" ) {
      VOpRatio=VOpRatio+Res
      next  
    }

    if( info=="Memory Size (MB)" ) {
      MemorySize=MemorySize+Res
      next
    }

    if( info=="MIPS" ) {
      MIPS=MIPS+Res
      next  
    }

    if( info=="I-Cache (sec)" ) {
      ICache=ICache+Res
      next
    }

    if( info=="O-Cache (sec)" ) {
      OCache=OCache+Res
      next  
    }

    if( info=="Bank (sec)" ) {
      Bank=Bank+Res
      next
    }

  }
# Get information with time output on other hosts
# ksh time
  else if (match($0, ".*s real .*s user .*s system"))
  {
    myprint( "ksh" )
    counter=counter+1

    RealTime=(substr($1,1,match($1, "s")-1))
    UserTime=(substr($3,1,match($3, "s")-1))
    SysTime=(substr($5,1,match($5, "s")-1))

  }

# csh time
# 0.000+u 0.000+s 0:00.00 0.0%      0+0k 0+0io 0pf+0w
  else if (match($0, "[0-9.]+u [0-9.]+s .+"))
  {
    myprint( "csh" )
    counter=counter+1

    UserTime=(substr($1,1,match($1, "u")-1))
    SysTime=(substr($2,1,match($2, "s")-1))
    # si hours ?
    RealTime=(substr($3,1,match($3, ":")-1)*60. + substr($3,match($3, ":")+1))

  }
# linux system time
### example :
### 2.02user 6.60system 12:41.62elapsed 1%CPU (0avgtext+0avgdata 25520maxresident)k
### 1.31user 5.33system 2:47:39elapsed 0%CPU (0avgtext+0avgdata 30400maxresident)k
  else if (match($0, ".*user .*system .*elapsed .*CPU .*"))
  {
    myprint( "linux" )
    counter=counter+1

    UserTime=(substr($1,1,match($1, "user")-1))
    SysTime=(substr($2,1,match($2, "system")-1))
    ### RealTime=(substr($3,1,match($3, ":")-1)*60.+substr($3, match($3, ":")+1, match($3, "elapsed"))) what for hours????

    RealTime=0
    if (match($3, "[0-9]*:[0-9]*:[0-9]*elapsed"))
    {
      #### $3=2:47:39elapsed
      myprint( "linux elapsed in hours " )
      sub1=(match($3, ":")-1)
      sub2=sub1+2
      min=(substr($3,sub2))
      sub3=(match(min, ":")-1)
      sub4=sub3+2
      fin=(substr(min,sub4))
      sub5=(match(fin, "elapsed")-1)
      RealTime=((substr($3,1,sub1))*60+substr(min,1,sub3))*60+(substr(min,sub4,sub5))
      myprint( " real time seconds " RealTime )
    }
    else if (match($3, "[0-9]*:[0-9]*\\.[0-9]*elapsed"))
    {
      #### $3=12:41.62elapsed
      myprint( "linux elapsed in minutes " )
      sub1=(match($3, ":")-1)
      sub2=sub1+2
      fin=(substr($3,sub2))
      sub3=(match(fin, "elapsed")-1)
      RealTime=(substr($3,1,sub1))*60+(substr($3,sub2,sub3))
      myprint( " real time seconds " RealTime )
    }
    else if (match($3, "[0-9]*\\.[0-9]*elapsed"))
    {
      #### $3=41.62elapsed
      myprint( "linux elapsed in second " )
      RealTime=(substr($3,1,match($3, "elapsed")-1))
      myprint( " real time seconds " RealTime )
    }
    next
  }
# curie system time (if not linux) or vargas
### curie (with tab)
### real    9m38.96s
### user    0m4.45s
### sys     0m6.41s
### vargas (with space)
### real   2891.80
### user   0.28
### sys    0.45

  else if (match($1, "real"))
  {
    myprint( "curie ou vargas ?" )

    # real    9m38.96s or real   2891.80
    if (match($2, ".*s.*"))
    {
      myprint( "curie" )
      counter=counter+1
      ### real    9m38.96s
      ### real  5h9m38.96s
      RealTime=((substr($2,1,match($2, "h")-1)*60.)+substr($2,match($2, "h")+1,match($2, "m")-1))*60.+substr($2,match($2, "m")+1,match($2, "s"))
      next
    }
    else
    {
      myprint( "vargas" )
      counter=counter+1
   
      ### real 2891.80
      RealTime=$2
    next
    }
  }
  else if (match($1, "user"))
  {
    # user(tab)0m4.45s or user   0.28
    if (match($2, ".*s.*"))
    {
       myprint( "curie user" )
       UserTime=((substr($2,1,match($2, "h")-1)*60.)+substr($2,match($2, "h")+1,match($2, "m")-1))*60.+substr($2,match($2, "m")+1,match($2, "s"))
       next
    }
    else
    {
      myprint( "vargas user" )
      UserTime=$2
      next
    }
  }
  else if (match($1, "sys"))
  {
    # 0m6.41s or 0.45
    if (match($2, ".*s.*"))
    {
      myprint( "curie sys" )
      SysTime=((substr($2,1,match($2, "h")-1)*60.)+substr($2,match($2, "h")+1,match($2, "m")-1))*60.+substr($2,match($2, "m")+1,match($2, "s"))
      next
    }
    else
    {
      myprint( "vargas sys" )
      SysTime=$2
      next
    }
  }

# Get information with time output on SX hosts
# ksh time
  else if (match($0, "real *[0-9:.]*"))
  {
    myprint("ksh SX")
    Information_ksh_SX_found=1
    counter=counter+1

    RealTime=0
    if (match($2, "[0-9]*:[0-9]*:[0-9]*\\.[0-9]*"))
    {
	sub1=(match($2, ":")-1)
	sub2=sub1+2
	fin=(substr($2,sub2))
	sub3=(match(fin, ":")-1)+sub2
	RealTime=(substr($2,1,sub1))*60+(substr($2,sub2,sub3))
    }
    else if (match($2, "[0-9]*:[0-9]*\\.[0-9]*"))
    {
	RealTime=(substr($2,1,match($2, ":")-1))
    }
  }
  else if (Information_ksh_SX_found == 1 && match($0, "user *[0-9:.]*"))
  {
    myprint("ksh SX ")

    UserTime=0
    if (match($2, "[0-9]*:[0-9]*:[0-9]*\\.[0-9]*"))
    {
	sub1=(match($2, ":")-1)
	sub2=sub1+2
	fin=(substr($2,sub2))
	sub3=(match(fin, ":")-1)+sub2
	UserTime=(substr($2,1,sub1))*60+(substr($2,sub2,sub3))
    }
    else if (match($2, "[0-9]*:[0-9]*\\.[0-9]*"))
    {
	UserTime=(substr($2,1,match($2, ":")-1))
    }
  }
  else if (Information_ksh_SX_found == 1 && match($0, "sys *[0-9:.]*"))
  {
    myprint("ksh SX ")

    SysTime=0
    if (match($2, "[0-9]*:[0-9]*:[0-9]*\\.[0-9]*"))
    {
	sub1=(match($2, ":")-1)
	sub2=sub1+2
	fin=(substr($2,sub2))
	sub3=(match(fin, ":")-1)+sub2
	SysTime=(substr($2,1,sub1))*60+(substr($2,sub2,sub3))
    }
    else if (match($2, "[0-9]*:[0-9]*\\.[0-9]*"))
    {
	SysTime=(substr($2,1,match($2, ":")-1))
    }
  }

# RUN_DATE_BEGIN
if ($0 ~ /RunDateBegin.*=/) {
    myprint( "start date" )
    split($0,a,"=")
    RunDateBegin=a[2]
}

# RUN_DATE_END
if ($0 ~ /RunDateEnd.*=/) {
    myprint( "end date" )
    split($0,a,"=")
    RunDateEnd=a[2]
  }

}

#==========================
END {

    myprint("exit_value : " exit_value)
    myprint("counter    : " counter)

  if (! exit_value ) {
    if (counter > 0) { 
      myprint("=============================================")
      myprint("Additionnal results on Program Informations for " ARGV[1])
      myprint("Real Time (sec)        :" RealTime)
      myprint("User Time (sec)        :" UserTime)
      myprint("Sys  Time (sec)        :" SysTime)
      myprint("Vector Time (sec)      :" VectorTime)
      myprint("Inst. Count            :" InstCount)
      myprint("V. Inst. Count         :" VInstCount)
      myprint("V. Element Count       :" VElementCount)
      myprint("FLOP Count             :" FLOPCount)
      myprint("MOPS                   :" MOPS)
      myprint("MFLOPS                 :" MFLOPS)
      myprint("A.V. Length            :" AVLength)
      myprint("V. Op. Ratio (%)       :" VOpRatio)
      myprint("Memory Size (MB)       :" MemorySize)
      myprint("MIPS                   :" MIPS)
      myprint("I-Cache (sec)          :" ICache)
      myprint("O-Cache (sec)          :" OCache)
      myprint("Bank (sec)             :" Bank)

      myprint("Date of executables :")
      myprint("Start Time             :" RunDateBegin)
      myprint("End   Time             :" RunDateEnd)

      printf("%s %s %.5f %.5f %.5f", RunDateBegin, RunDateEnd, RealTime, UserTime, SysTime) 
      exit(0)
    }
    else
    { 
      exit(1)
    }
  }
  else
  { 
    exit(2)
  }
  
}
