  Z  2   k820309    ?          19.1        �Ad                                                                                                          
       /home/orchidee04/yangsu/Calibration/0_a_Final_calibration/modeles/ORCHIDEE/.config/ppsrc/parallel/xios_orchidee.f90 XIOS_ORCHIDEE              XIOS_ORCHIDEE_COMM_INIT XIOS_ORCHIDEE_INIT XIOS_ORCHIDEE_CHANGE_CONTEXT XIOS_ORCHIDEE_UPDATE_CALENDAR XIOS_ORCHIDEE_CONTEXT_FINALIZE XIOS_ORCHIDEE_FINALIZE XIOS_ORCHIDEE_OK gen@XIOS_ORCHIDEE_SEND_FIELD                                                     
                                                           
                @      @�                                  
                                                           
                                                           
                                                           
       NVM                                                     
       NSTM CHECK_WATERBAL DIAGLEV                                                     
       NGRND NSLM NBDL                                                	     
       IOGET_CALENDAR JU2YMDS                                                        u #XIOS_ORCHIDEE_SEND_FIELD_R1D 
   #XIOS_ORCHIDEE_SEND_FIELD_R2D    #XIOS_ORCHIDEE_SEND_FIELD_R3D    #         @     @X                             
                    #FIELD_ID    #FIELD              
                                                    1           
  @                                                 
              &                                           #         @     @X                                                 #FIELD_ID    #FIELD              
                                                    1        0  
 @                                                 
              &                   &                                           #         @     @X                                                 #FIELD_ID    #FIELD              
                                                    1        0  
 @                                                 
              &                   &                   &                                                         @�                                      u #IOGET_CALENDAR_REAL1    #IOGET_CALENDAR_REAL2    #IOGET_CALENDAR_STR    #         @     @                                                #LONG_YEAR                                                    
       #         @     @                                                #LONG_YEAR    #LONG_DAY                                                    
                                                       
       #         @     @                                               #STR                                                                  1 #         @                                                       #COMM_LOCAL                                                           #         @                                                       #MPI_COMM_ORCH    #DATE0    #YEAR    #MONTH     #DAY !   #LON_MPI "   #LAT_MPI %   #SOILTH_LEV &             
                                                      
  @                                   
                
                                                      
                                                       
                                 !                    
                                 "                    
      p        5 r #   p          5 r #     5 r $       5 r #     5 r $                              
                                 %                    
      p        5 r #   p          5 r #     5 r $       5 r #     5 r $                              
                                 &                    
    p          5 r '       5 r '                     #         @                                   (                    #NEW_CONTEXT )             
                                )                    1 #         @                                   *                    #ITAU_SECHIBA +             
                                 +           #         @                                   ,                     #         @                                   -                               @@                               .                         @                             '                         @                              $                         @                             #               �   �      fn#fn #   *  �   b   uapp(XIOS_ORCHIDEE      @   J  DEFPREC    D  @   J  CONSTANTES_VAR &   �  @   j  MOD_ORCHIDEE_PARA_VAR ,   �  @   J  MOD_ORCHIDEE_TRANSFERT_PARA      @   J  IOIPSL_PARA #   D  D   J  PFT_PARAMETERS_VAR $   �  \   J  CONSTANTES_SOIL_VAR "   �  P   j  VERTICAL_SOIL_VAR    4  W   J  IOIPSL -   �  �       gen@XIOS_ORCHIDEE_SEND_FIELD -   1  a      XIOS_ORCHIDEE_SEND_FIELD_R1D 6   �  L   a   XIOS_ORCHIDEE_SEND_FIELD_R1D%FIELD_ID 3   �  �   a   XIOS_ORCHIDEE_SEND_FIELD_R1D%FIELD -   j  a      XIOS_ORCHIDEE_SEND_FIELD_R2D 6   �  L   a   XIOS_ORCHIDEE_SEND_FIELD_R2D%FIELD_ID 3     �   a   XIOS_ORCHIDEE_SEND_FIELD_R2D%FIELD -   �  a      XIOS_ORCHIDEE_SEND_FIELD_R3D 6     L   a   XIOS_ORCHIDEE_SEND_FIELD_R3D%FIELD_ID 3   h  �   a   XIOS_ORCHIDEE_SEND_FIELD_R3D%FIELD ,   $	  �       gen@IOGET_CALENDAR+CALENDAR .   �	  W      IOGET_CALENDAR_REAL1+CALENDAR 8   
  @   a   IOGET_CALENDAR_REAL1%LONG_YEAR+CALENDAR .   G
  e      IOGET_CALENDAR_REAL2+CALENDAR 8   �
  @   a   IOGET_CALENDAR_REAL2%LONG_YEAR+CALENDAR 7   �
  @   a   IOGET_CALENDAR_REAL2%LONG_DAY+CALENDAR ,   ,  Q      IOGET_CALENDAR_STR+CALENDAR 0   }  L   a   IOGET_CALENDAR_STR%STR+CALENDAR (   �  X       XIOS_ORCHIDEE_COMM_INIT 3   !  @   a   XIOS_ORCHIDEE_COMM_INIT%COMM_LOCAL #   a  �       XIOS_ORCHIDEE_INIT 1     @   a   XIOS_ORCHIDEE_INIT%MPI_COMM_ORCH )   O  @   a   XIOS_ORCHIDEE_INIT%DATE0 (   �  @   a   XIOS_ORCHIDEE_INIT%YEAR )   �  @   a   XIOS_ORCHIDEE_INIT%MONTH '     @   a   XIOS_ORCHIDEE_INIT%DAY +   O  �   a   XIOS_ORCHIDEE_INIT%LON_MPI +   #  �   a   XIOS_ORCHIDEE_INIT%LAT_MPI .   �  �   a   XIOS_ORCHIDEE_INIT%SOILTH_LEV -   �  Y       XIOS_ORCHIDEE_CHANGE_CONTEXT 9   �  L   a   XIOS_ORCHIDEE_CHANGE_CONTEXT%NEW_CONTEXT .   0  Z       XIOS_ORCHIDEE_UPDATE_CALENDAR ;   �  @   a   XIOS_ORCHIDEE_UPDATE_CALENDAR%ITAU_SECHIBA /   �  H       XIOS_ORCHIDEE_CONTEXT_FINALIZE '     H       XIOS_ORCHIDEE_FINALIZE !   Z  @       XIOS_ORCHIDEE_OK (   �  @      NGRND+VERTICAL_SOIL_VAR ,   �  @      JJ_NB+MOD_ORCHIDEE_PARA_VAR ,     @      IIM_G+MOD_ORCHIDEE_PARA_VAR 