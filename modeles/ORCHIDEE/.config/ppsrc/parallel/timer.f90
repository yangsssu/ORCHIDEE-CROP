










! Timer functions to calculate MPI use speed up.

!-
!< $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_parallel/timer.f90 $ 
!< $Date: 2014-03-26 10:19:44 +0100 (Wed, 26 Mar 2014) $
!< $Author: josefine.ghattas $
!< $Revision: 1993 $
!-

MODULE timer

  USE mod_orchidee_para_var, ONLY : numout
  
  INTEGER, PARAMETER :: nb_timer=2
  INTEGER, PARAMETER :: timer_global=1
  INTEGER, PARAMETER :: timer_mpi=2
  INTEGER, PARAMETER :: stopped = 1
  INTEGER, PARAMETER :: running = 2
  INTEGER, PARAMETER :: suspended = 3
  
  DOUBLE PRECISION, DIMENSION(nb_timer),SAVE :: cpu_timer
  DOUBLE PRECISION, DIMENSION(nb_timer),SAVE :: real_timer
  INTEGER, DIMENSION(nb_timer),SAVE :: timer_state
  DOUBLE PRECISION, DIMENSION(nb_timer),SAVE :: last_cpu_time
  INTEGER, DIMENSION(nb_timer),SAVE :: last_real_time
  
  
  
  
  CONTAINS
  
  SUBROUTINE init_timer
  IMPLICIT NONE
    
    cpu_timer(:)=0.
    real_timer(:)=0.
    timer_state(:)=stopped
    last_cpu_time(:)=0.
    last_real_time(:)=0
    
  END SUBROUTINE init_timer
  
  
  SUBROUTINE start_timer(no_timer)
  IMPLICIT NONE
     INTEGER :: no_timer
     DOUBLE PRECISION :: x
     
     IF (timer_state(no_timer)/=stopped) THEN
       STOP 'start_timer :: timer is already running or suspended'
     ELSE
        timer_state(no_timer)=running
     ENDIF
      
     cpu_timer(no_timer)=0. 
     real_timer(no_timer)=0.
     x=Diff_real_time(no_timer)
     x=Diff_cpu_time(no_timer)
     
  END SUBROUTINE start_timer
  
  
  
  SUBROUTINE stop_timer(no_timer)
  IMPLICIT NONE
    INTEGER :: no_timer
    
     IF (timer_state(no_timer)==running) THEN
        CALL suspend_timer(no_timer)
     ELSE IF (timer_state(no_timer)==stopped) THEN
       WRITE(numout,*) 'stop_timer :: timer is already stopped'
     ENDIF

     timer_state(no_timer)=stopped

  END SUBROUTINE stop_timer
  
  
  
  SUBROUTINE resume_timer(no_timer)
  IMPLICIT NONE
    INTEGER :: no_timer
    DOUBLE PRECISION :: x
     IF (timer_state(no_timer)/=suspended) THEN
       STOP 'resume_timer :: timer is not suspended'
     ELSE
        timer_state(no_timer)=running
     ENDIF
  
     x=Diff_cpu_time(no_timer)
     x=Diff_real_time(no_timer)  
  
  END SUBROUTINE resume_timer
  
  
  
  SUBROUTINE suspend_timer(no_timer)
  
    IMPLICIT NONE
    INTEGER :: no_timer
    
     IF (timer_state(no_timer)/=running) THEN
       STOP 'suspend_timer :: timer is not running'
     ELSE
        timer_state(no_timer)=suspended
     ENDIF
  
     cpu_timer(no_timer)=cpu_timer(no_timer)+Diff_cpu_time(no_timer)
     real_timer(no_timer)=real_timer(no_timer)+Diff_real_time(no_timer)
  
  END SUBROUTINE suspend_timer
  
  
  FUNCTION diff_real_time(no_timer)
  IMPLICIT NONE
    INTEGER :: no_timer
    DOUBLE PRECISION :: Diff_real_Time
    integer :: Last_Count,count,count_rate,count_max
    
    Last_Count=Last_real_time(no_timer)
    
    call system_clock(count,count_rate,count_max)
    if (Count>=Last_Count) then
      Diff_real_time=(1.*(Count-last_Count))/count_rate
    else
      Diff_real_time=(1.*(Count-last_Count+Count_max))/count_rate
    endif
    Last_real_time(no_timer)=Count 
    
  END FUNCTION diff_real_time
  
  function Diff_Cpu_Time(no_timer)
  implicit none
    INTEGER :: no_timer
    DOUBLE PRECISION :: Diff_Cpu_Time
    DOUBLE PRECISION :: Last_Count,Count
    
    Last_Count=Last_cpu_time(no_timer)
    
    call cpu_time(Count)
    Diff_Cpu_Time=Count-Last_Count
    Last_cpu_time(no_timer)=Count 
    
  end function Diff_Cpu_Time
  
  FUNCTION Get_cpu_time(no_timer)
  IMPLICIT NONE
  INTEGER :: no_timer
  DOUBLE PRECISION :: Get_cpu_time
  
    IF (timer_state(no_timer)==running) THEN
      CALL suspend_timer(no_timer)
      Get_cpu_time=cpu_timer(no_timer)
      CALL resume_timer(no_timer)
    ELSE
      Get_cpu_time=cpu_timer(no_timer)
    ENDIF
    
  END FUNCTION Get_cpu_time
  
  FUNCTION Get_real_time(no_timer)
  IMPLICIT NONE
  INTEGER :: no_timer
  DOUBLE PRECISION :: Get_real_time
  
    IF (timer_state(no_timer)==running) THEN
      CALL suspend_timer(no_timer)
      Get_real_time=real_timer(no_timer)
      CALL resume_timer(no_timer)
    ELSE
      Get_real_time=real_timer(no_timer)
    ENDIF
  
  END FUNCTION Get_real_time
  
END MODULE Timer
  
