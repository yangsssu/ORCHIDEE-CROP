










! INITIALIZATION OF VARIABLES REGARDING THE CROP CYCLE
! 27/06/2013---xcw 

subroutine Stics_init(&
               kjpindex        ,&
               !nvm             ,&
               f_crop_init     ,&   
               f_crop_recycle      ,&   
               in_cycle                ,&   
               f_sen_lai                ,&   
               onarretesomcourdrp      ,&   
!               nlevobs                 ,&
!               namfobs                 ,&
!               nfloobs                 ,&
!               nlanobs                 ,&
!               nlaxobs                 ,&
!               nmatobs                 ,&
!               nrecobs                 ,&
!               nsenobs                 ,&
!               ndrpobs                 ,&
               nsendltams              ,&
               nsendltai               ,&
               nsenpfeuilverte         ,&
               nsendurvie              ,&
               nsenndurvie             ,&
               densiteequiv            ,&
               nplt                    ,&
               tursla                  ,&
               ssla                     ,&
               pfeuilverte             ,&
               bsenlai                 ,&
               zrac                    ,&
               nrec                    ,& 
               nlan                    ,&
               tcult                   ,&
               udevair                 ,&
               udevcult                ,&
               ndrp                    ,&
               rfvi                    ,&
               nlev                    ,&
               nger                    ,&
               etatvernal              ,&
               caljvc                  ,&
               rfpi                    ,&
               upvt                    ,&
               utp                     ,&
               somcour                 ,&
               somcourdrp              ,&
               somcourutp              ,&
               tdevelop                ,&
               somtemp                 ,&
               somcourfauche           ,&
               stpltger                ,&
               R_stamflax              ,&
               R_stlaxsen              ,&
               R_stsenlan              ,&
               stlevflo                ,&
               nflo                    ,&
               R_stlevdrp              ,&
               R_stflodrp              ,&
               R_stdrpmat              ,&
               nmat                    ,&
               nlax                    ,&
               nrecbutoir              ,&
               group                   ,&
               ndebdes                 ,&
               R_stdrpdes              ,&
               densite                 ,&
               densitelev              ,&
               coeflev                 ,&
               densiteger              ,&
               somelong                 ,&
               somger                  ,&
               humectation             ,&
               nbjhumec                ,&
               somtemphumec            ,&
               stpltlev                ,&
               namf                    ,&
               stmatrec                ,&
               tustress                ,&
               slai                     ,&
               somfeuille              ,&
               pdlai                   ,&
               nbfeuille               ,&
               reajust                 ,&
               ulai                    ,&
               pdulai                  ,&
               efdensite               ,&
               tempeff                 ,&
               nstopfeuille            ,&
               deltai                  ,&
               svmax                    ,&
               nsen                    ,&
               laisen                  ,&
               pdlaisen                ,&
               dltaisenat              ,&
               nsencour                ,&
               dltamsen                ,&
               dltaisen                ,&
               fgellev                 ,&
               gelee                   ,&
               fstressgel              ,&
               R_stlevamf              ,&
               dernier_n               ,&
               durvieI                 ,&
               durvie                  ,&
               ndebsen                 ,&
               somsenreste             ,&
               shumrel                  ,&
               swfac                   ,&
               turfac                  ,&
               senfac                  ,&  
               mafeuiljaune            ,&
               msneojaune              ,&              
               v_dltams                ,&
               fgelflo                 ,&
               pdircarb                ,&
               ircarb                  ,&
               nbgrains                ,&
               pgrain                  ,&
               vitmoy                  ,&
               nbgraingel              ,&
               pgraingel               ,&
               dltags                  ,&
               ftempremp               ,&
               magrain                 ,&
               pdmagrain               ,&
               nbj0remp                ,&
               pdsfruittot             ,&
               repracmax               ,&
               repracmin               ,&
               kreprac                 ,&
               somtemprac              ,&
               urac                    ,&
               reprac                  ,&
               nstoprac                ,&
               c_reserve               ,&
               c_leafb                 ,&
               !biomass                 ,&
               deltgrain               ,&
               gslen                   ,&
               drylen, &
               histgrowthset, hist_sencourset, hist_latestset, doyhiststset, &
               nboxmax,box_ulai, box_ndays, box_lai, box_lairem, box_tdev, box_biom, box_biomrem, box_durage, box_somsenbase)                


  !USE stomate_io
  USE pft_parameters
  USE constantes


  implicit none

  ! DECLARATION
  integer,intent(in)                             ::         kjpindex
  !integer,intent(in)                             ::         nvm
  integer,intent(in)                             ::         nboxmax
  logical, intent(inout)::         f_crop_init        
  logical,dimension(kjpindex, nvm), intent(inout)::         f_crop_recycle         
  logical,dimension(kjpindex, nvm), intent(inout)::         in_cycle         
  logical,dimension(kjpindex, nvm), intent(inout)::         f_sen_lai         
  logical,dimension(kjpindex, nvm), intent(inout)::         onarretesomcourdrp         
!  integer,dimension(kjpindex, nvm), intent(inout)::         nlevobs                 
!  integer,dimension(kjpindex, nvm), intent(inout)::         namfobs                 
!  integer,dimension(kjpindex, nvm), intent(inout)::         nfloobs                 
!  integer,dimension(kjpindex, nvm), intent(inout)::         nlanobs                 
!  integer,dimension(kjpindex, nvm), intent(inout)::         nlaxobs                 
!  integer,dimension(kjpindex, nvm), intent(inout)::         nmatobs                 
!  integer,dimension(kjpindex, nvm), intent(inout)::         nrecobs                 
!  integer,dimension(kjpindex, nvm), intent(inout)::         nsenobs                 
!  integer,dimension(kjpindex, nvm), intent(inout)::         ndrpobs                 
  !
  real,dimension(kjpindex, nvm), intent(inout)::         nsendltams              
  real,dimension(kjpindex, nvm), intent(inout)::         nsendltai               
  real,dimension(kjpindex, nvm), intent(inout)::         nsenpfeuilverte         
  real,dimension(kjpindex, nvm), intent(inout)::         nsendurvie              
  real,dimension(kjpindex, nvm), intent(inout)::         nsenndurvie             
  real,dimension(kjpindex, nvm), intent(inout)::         densiteequiv            
  integer,dimension(kjpindex, nvm), intent(inout)::         nplt                    
  real,dimension(kjpindex, nvm), intent(inout)::         tursla                  
  real,dimension(kjpindex, nvm), intent(inout)::         ssla                     
  real,dimension(kjpindex, nvm), intent(inout)::         pfeuilverte             
  real,dimension(kjpindex, nvm), intent(inout)::         bsenlai                 
  
  ! STICS::LAIdev::DEVELOPMENT
  real,dimension(kjpindex, nvm), intent(inout)::         zrac                    
  integer,dimension(kjpindex, nvm), intent(inout)::         nrec                     
  integer,dimension(kjpindex, nvm), intent(inout)::         nlan                    
  real,dimension(kjpindex, nvm), intent(inout)::         tcult                   
  real,dimension(kjpindex, nvm), intent(inout)::         udevair                 
  real,dimension(kjpindex, nvm), intent(inout)::         udevcult                
  integer,dimension(kjpindex, nvm), intent(inout)::         ndrp                    
  real,dimension(kjpindex, nvm), intent(inout)::         rfvi                    
  integer,dimension(kjpindex, nvm), intent(inout)::         nlev                    
  integer,dimension(kjpindex, nvm), intent(inout)::         nger                    
  logical,dimension(kjpindex, nvm), intent(inout)::         etatvernal              
  real,dimension(kjpindex, nvm), intent(inout)::         caljvc                  
  real,dimension(kjpindex, nvm), intent(inout)::         rfpi                    
  real,dimension(kjpindex, nvm), intent(inout)::         upvt                    
  real,dimension(kjpindex, nvm), intent(inout)::         utp                     
  real,dimension(kjpindex, nvm), intent(inout)::         somcour                 
  real,dimension(kjpindex, nvm), intent(inout)::         somcourdrp              
  real,dimension(kjpindex, nvm), intent(inout)::         somcourutp              
  real,dimension(kjpindex, nvm), intent(inout)::         tdevelop                
  real,dimension(kjpindex, nvm), intent(inout)::         somtemp                 
  real,dimension(kjpindex, nvm), intent(inout)::         somcourfauche           
  real,dimension(kjpindex, nvm), intent(inout)::         stpltger                
  real,dimension(kjpindex, nvm), intent(inout)::         R_stamflax              
  real,dimension(kjpindex, nvm), intent(inout)::         R_stlaxsen              
  real,dimension(kjpindex, nvm), intent(inout)::         R_stsenlan              
  real,dimension(kjpindex, nvm), intent(inout)::         stlevflo                
  integer,dimension(kjpindex, nvm), intent(inout)::         nflo                    
  real,dimension(kjpindex, nvm), intent(inout)::         R_stlevdrp              
  real,dimension(kjpindex, nvm), intent(inout)::         R_stflodrp              
  real,dimension(kjpindex, nvm), intent(inout)::         R_stdrpmat              
  integer,dimension(kjpindex, nvm), intent(inout)::         nmat                    
  integer,dimension(kjpindex, nvm), intent(inout)::         nlax                    
  integer,dimension(kjpindex, nvm), intent(inout)::         nrecbutoir              
  real,dimension(kjpindex, nvm), intent(inout)::         group                   
  integer,dimension(kjpindex, nvm), intent(inout)::         ndebdes                 
  real,dimension(kjpindex, nvm), intent(inout)::         R_stdrpdes              
  real,dimension(kjpindex, nvm), intent(inout)::         densite                 
  real,dimension(kjpindex, nvm), intent(inout)::         densitelev
  real,dimension(kjpindex, nvm), intent(inout)::         coeflev
              
  real,dimension(kjpindex, nvm), intent(inout)::         densiteger              
  real,dimension(kjpindex, nvm), intent(inout)::         somelong                 
  real,dimension(kjpindex, nvm), intent(inout)::         somger                  
  logical,dimension(kjpindex, nvm), intent(inout)::         humectation             
  integer,dimension(kjpindex, nvm), intent(inout)::         nbjhumec                
  real,dimension(kjpindex, nvm), intent(inout)::         somtemphumec            
  real,dimension(kjpindex, nvm), intent(inout)::         stpltlev                
  integer,dimension(kjpindex, nvm), intent(inout)::         namf                    
  real,dimension(kjpindex, nvm), intent(inout)::         stmatrec                
  ! STICS::LAIdev:: LAI calculation
  real,dimension(kjpindex, nvm), intent(inout)::         tustress                
  real,dimension(kjpindex, nvm), intent(inout)::         slai                     
  real,dimension(kjpindex, nvm), intent(inout)::         somfeuille              
  real,dimension(kjpindex, nvm), intent(inout)::         pdlai                   
  integer,dimension(kjpindex, nvm), intent(inout)::         nbfeuille               
  real,dimension(kjpindex, nvm), intent(inout)::         reajust                 
  real,dimension(kjpindex, nvm), intent(inout)::         ulai                    
  real,dimension(kjpindex, nvm), intent(inout)::         pdulai                  
  real,dimension(kjpindex, nvm), intent(inout)::         efdensite               
  real,dimension(kjpindex, nvm), intent(inout)::         tempeff                 
  integer,dimension(kjpindex, nvm), intent(inout)::         nstopfeuille            
  real,dimension(kjpindex, nvm), intent(inout)::         deltai                  
  real,dimension(kjpindex, nvm), intent(inout)::         svmax                    
  integer,dimension(kjpindex, nvm), intent(inout)::         nsen                    
  real,dimension(kjpindex, nvm), intent(inout)::         laisen                  
  real,dimension(kjpindex, nvm), intent(inout)::         pdlaisen                
  real,dimension(kjpindex, nvm), intent(inout)::         dltaisenat              
  ! STICS:: LAIdev:: LAI senescence 
  integer,dimension(kjpindex, nvm), intent(inout)::         nsencour                
  real,dimension(kjpindex, nvm), intent(inout)::         dltamsen                
  real,dimension(kjpindex, nvm), intent(inout)::         dltaisen                
  real,dimension(kjpindex, nvm), intent(inout)::         fgellev                 
  logical,dimension(kjpindex, nvm), intent(inout)::         gelee                   
  real,dimension(kjpindex, nvm), intent(inout)::         fstressgel              
  real,dimension(kjpindex, nvm), intent(inout)::         R_stlevamf              
  integer,dimension(kjpindex, nvm), intent(inout)::         dernier_n               
  real,dimension(kjpindex, nvm), intent(inout)::         durvieI                 
  real,dimension(kjpindex, nvm), intent(inout)::         durvie                  
  integer,dimension(kjpindex, nvm), intent(inout)::         ndebsen                 
  real,dimension(kjpindex, nvm), intent(inout)::         somsenreste
  ! STICS:: LAIdev:: STRESS             
  real,dimension(kjpindex, nvm), intent(inout)::         shumrel                  
  real,dimension(kjpindex, nvm), intent(inout)::         swfac                   
  real,dimension(kjpindex, nvm), intent(inout)::         turfac                  
  real,dimension(kjpindex, nvm), intent(inout)::         senfac                
 
  real,dimension(kjpindex, nvm), intent(inout)::         mafeuiljaune                
  real,dimension(kjpindex, nvm), intent(inout)::         msneojaune                
  ! STICS:: CARBON ALLOCATION

  real,dimension(kjpindex, nvm, 60), intent(inout)::         v_dltams                
  real,dimension(kjpindex, nvm), intent(inout)::         fgelflo                
  real,dimension(kjpindex, nvm), intent(inout)::         pdircarb                
  real,dimension(kjpindex, nvm), intent(inout)::         ircarb                
  real,dimension(kjpindex, nvm), intent(inout)::         nbgrains                
  real,dimension(kjpindex, nvm), intent(inout)::         pgrain              
  real,dimension(kjpindex, nvm), intent(inout)::         vitmoy                
  real,dimension(kjpindex, nvm), intent(inout)::         nbgraingel                
  real,dimension(kjpindex, nvm), intent(inout)::         pgraingel                
  real,dimension(kjpindex, nvm), intent(inout)::         dltags                
  real,dimension(kjpindex, nvm), intent(inout)::         ftempremp                
  real,dimension(kjpindex, nvm), intent(inout)::         magrain                
  real,dimension(kjpindex, nvm), intent(inout)::         pdmagrain               
  integer,dimension(kjpindex, nvm), intent(inout)::         nbj0remp                 
  real,dimension(kjpindex, nvm), intent(inout)::         pdsfruittot               
  real,dimension(kjpindex, nvm), intent(inout)::         repracmax               
  real,dimension(kjpindex, nvm), intent(inout)::         repracmin               
  real,dimension(kjpindex, nvm), intent(inout)::         kreprac               
  real,dimension(kjpindex, nvm), intent(inout)::         somtemprac               
  real,dimension(kjpindex, nvm), intent(inout)::         urac              
  real,dimension(kjpindex, nvm), intent(inout)::         reprac               

  integer,dimension(kjpindex, nvm), intent(inout)::         nstoprac                 
  real,dimension(kjpindex, nvm), intent(inout)::         c_reserve               
  real,dimension(kjpindex, nvm), intent(inout)::         c_leafb            
  !real,dimension(kjpindex, nvm, nparts ), intent(inout)::         biomass           
  real,dimension(kjpindex, nvm), intent(inout)::         deltgrain          
  integer,dimension(kjpindex, nvm), intent(inout)::         gslen                 
  integer,dimension(kjpindex, nvm), intent(inout)::         drylen                 
  real,dimension(kjpindex, nvm, 300, 5), intent(inout) :: histgrowthset
  integer, dimension(kjpindex,nvm), intent(inout) :: hist_sencourset
  integer, dimension(kjpindex,nvm), intent(inout) :: hist_latestset
  integer, dimension(kjpindex,nvm), intent(inout) :: doyhiststset

  integer, dimension(kjpindex,nvm,nboxmax), intent(inout) :: box_ndays
  real, dimension(kjpindex,nvm,nboxmax), intent(inout)    :: box_lai
  real, dimension(kjpindex,nvm,nboxmax), intent(inout)    :: box_lairem
  real, dimension(kjpindex,nvm,nboxmax), intent(inout)    :: box_tdev
  real, dimension(kjpindex,nvm,nboxmax), intent(inout)    :: box_biom
  real, dimension(kjpindex,nvm,nboxmax), intent(inout)    :: box_biomrem
  real, dimension(kjpindex,nvm,nboxmax), intent(inout)    :: box_durage
  real, dimension(kjpindex,nvm,nboxmax), intent(inout)    :: box_somsenbase
  real, dimension(nvm,nboxmax), intent(inout)             :: box_ulai
  ! local variables
  integer    :: j, ip, k, mid
  real       :: uinflex

 
  ! 
  DO j= 1, nvm
     
     DO ip = 1, kjpindex 
       IF (f_crop_init .OR. f_crop_recycle(ip, j)) THEN  
          

          ! LAIdev :: FORCED RUN VARIABLES
          in_cycle(ip, j) = .FALSE.
          f_sen_lai(ip, j) = .TRUE.
          onarretesomcourdrp(ip, j) = .TRUE.
!          nlevobs(ip, j) = 999
!          namfobs(ip, j) = 999   
!          nfloobs(ip, j) = 999
!          nlanobs(ip, j) = 999
!          nlaxobs(ip, j) = 999
!          nmatobs(ip, j) = 999
!          nrecobs(ip, j) = 999
!          nsenobs(ip, j) = 999
!          ndrpobs(ip, j) = 999
        
          ! LAIdev ::  SPECIFIC
          nsendltams(ip, j) = 0.
          nsendltai(ip, j) = 0.
          nsenpfeuilverte(ip, j) = 0.
          nsendurvie(ip, j) = 0.
          nsenndurvie(ip, j) = 0.
          densiteequiv(ip, j) = 0.
          nplt(ip, j) = 0
          tursla(ip, j) = 1.
          ssla(ip, j) = 0.
          pfeuilverte(ip, j) = 0.
          bsenlai(ip, j) = 0.

          ! LAIdev ::  DEVELOPMENT 
          zrac(ip, j) = 0.
          nrec(ip, j) = 0
          nlan(ip, j) = 0
          tcult(ip, j) = 0.
          udevair(ip, j) = 0.
          udevcult(ip, j) = 0.
          ndrp(ip, j) = 0
          rfvi(ip, j) = 0.
          nlev(ip, j) = 0
          nger(ip, j) = 0
          etatvernal(ip, j) = .FALSE.
          caljvc(ip, j) = 0.
          rfpi(ip, j) = 0.
          upvt(ip, j) = 0.
          utp(ip, j) = 0.
          somcour(ip, j) = 0.
          somcourdrp(ip, j) = 0.
          somcourutp(ip, j) = 0.
          tdevelop(ip, j) = 0.
          somtemp(ip, j) = 0.
          somcourfauche(ip, j) = 0.
          stpltger(ip, j) = SP_stpltger(j)
          R_stamflax(ip, j) = SP_stamflax(j) 
          R_stlaxsen(ip, j) = SP_stlaxsen(j)
          R_stsenlan(ip, j) = SP_stsenlan(j)
          stlevflo(ip, j) = SP_stlevdrp(j) - SP_stflodrp(j)
          nflo(ip, j) = 0
          R_stlevdrp(ip, j) = SP_stlevdrp(j)
          R_stflodrp(ip, j) = SP_stflodrp(j)
          R_stdrpmat(ip, j) = SP_stdrpmat(j)
          nmat(ip, j) = 0
          nlax(ip, j) = 0
          nrecbutoir(ip, j) = 999
          group(ip, j) = 0.
          ndebdes(ip, j) = 0
          R_stdrpdes(ip, j) = SP_stdrpdes(j)
          densite(ip, j) = 0.
          densitelev(ip, j) = 0.
          coeflev(ip, j) = 1.
          densiteger(ip, j) = 0.
          somelong(ip, j) = 0.
          somger(ip, j) = 0.
          humectation(ip, j) = .FALSE.
          nbjhumec(ip, j) = 0
          somtemphumec(ip, j) = 0.
          stpltlev(ip, j) = 0.
          namf(ip, j) = 0
          stmatrec(ip, j) = 0.
          
          ! LAIdev ::  LAI calculation
          tustress(ip, j) = 1.
          slai(ip, j) = 0.
          somfeuille(ip, j) = 0.
          pdlai(ip, j) = 0.
          nbfeuille(ip, j) = 0
          reajust(ip, j) = 0.
          ulai(ip, j) = 0.
          pdulai(ip, j) = 0.
          efdensite(ip, j) = 0.
          tempeff(ip, j) = 0.
          nstopfeuille(ip, j) = 0
          deltai(ip, j) = 0.
          svmax(ip, j) = 0.
          nsen(ip, j) = 0
          laisen(ip, j) = 0.
          pdlaisen(ip, j) = 0.
          dltaisenat(ip, j) = 0.
          
          ! LAIdev :: LAI senescence
          nsencour(ip, j) = 0
          dltamsen(ip, j) = 0.
          dltaisen(ip, j) = 0.
          fgellev(ip, j) = 0.
          gelee(ip, j) = .FALSE.
          fstressgel(ip, j) = 0.
          R_stlevamf(ip, j) = SP_stlevamf(j)
          dernier_n(ip, j) = 0
          durvieI(ip, j) = 0.
          durvie(ip, j) = 0.
          ndebsen(ip, j) = 0
          somsenreste(ip, j) = 0.
          
          IF (any(ok_LAIdev(:)) .AND. any(SP_codlainet(:) == 3)) THEN
!              write(*,*) 'xuhui: box module to be initialize ', j
              box_ndays(ip,j,:) = 0
              box_lai(ip,j,:) = 0.
              box_lairem(ip,j,:) = 0.
              box_tdev(ip,j,:) = 0.
              box_biom(ip,j,:) = 0.
              box_biomrem(ip,j,:) = 0.
              box_durage(ip,j,:) = 0.
              box_somsenbase(ip,j,:) = 0.
!              write(*,*) 'xuhui: box module initialized ', j
          ENDIF
          ! LAIdev :: STRESS
           
          shumrel(ip, j) = 0.
          swfac(ip, j) = 1.
          turfac(ip, j) = 1.
          senfac(ip, j) = 1.

          mafeuiljaune(ip, j) = 0.
          msneojaune(ip, j) = 0.
          ! STICS: CARBON ALLOCATION 

          v_dltams(ip, j, :) = 0.
          fgelflo(ip, j) = 1.
          pdircarb(ip, j) = 0.
          ircarb(ip, j) = 0.
          nbgrains(ip, j) = 0.
          pgrain(ip, j) = 0.
          vitmoy(ip, j) = 0.
          nbgraingel(ip, j) = 0.
          pgraingel(ip, j) = 0.
          dltags(ip, j) = 0.
          ftempremp(ip, j) = 0.
          magrain(ip, j) = 0.
          pdmagrain(ip, j) = 0.
          nbj0remp(ip, j) = 0
          pdsfruittot(ip, j) = 0.
          repracmax(ip, j) = 0.
          repracmin(ip, j) = 0.
          kreprac(ip, j) = 0.
          somtemprac(ip, j) = 0.
          urac(ip, j) = 0.
          reprac(ip, j) = 0.
          nstoprac(ip, j) = 0
          c_reserve(ip, j) = 0. 
          c_leafb(ip, j)= 0. 
          deltgrain(ip, j) = 0.
          gslen(ip, j) = 0 
          drylen(ip, j) = 0 
          ! FINISH THE INITIALIZATION AND RESET THE VALUE
          IF (any(ok_LAIdev(:)) .AND. any(SP_codlainet(:) == 2)) THEN
              histgrowthset(ip,j,:,:) = 0.
              hist_sencourset(ip,j) = 0
              hist_latestset(ip,j) = 0
              doyhiststset(ip,j) = 0
          ENDIF
          f_crop_recycle(ip, j) = .FALSE.

        ENDIF
     ENDDO ! ip
  ENDDO ! j
    
!    write(*,*) 'xuhui: initialized boxulai'
    IF (any(ok_LAIdev(:)) .AND. any(SP_codlainet(:)==3)) THEN
        box_ulai(:,:) = 0.
        DO j=1,nvm
    !        IF (f_crop_init) THEN
!            IF (f_crop_init .OR. any(f_crop_recycle(:, j))) THEN
                if (SP_nbox(j) < 2) then
!                    write(*,*) 'xuhui: box_ulai not allocated for ', j, 'with nbox ', SP_nbox(j)
                else    
                    uinflex = SP_vlaimax(j)
                    mid = floor(real(SP_nbox(j))/2)
                    if (mid<1) then
                        mid=1
                    elseif (mid .eq. SP_nbox(j)) then
                        mid=SP_nbox(j)-1
                    endif
                    do k=1,mid
                        box_ulai(j,k) = 1+(k-1)*((uinflex-1)/real(mid))
                    enddo
                    do k=mid+1,SP_nbox(j)
                        box_ulai(j,k) = uinflex + (k-mid-1)*(3-uinflex)/real(SP_nbox(j)-mid)
                    enddo
!                    write(*,*) 'xuhui: box_ulai for pft ', j, ': ',box_ulai(j,:)
                endif
                
 !           ENDIF
        ENDDO
    ENDIF

end subroutine Stics_init

