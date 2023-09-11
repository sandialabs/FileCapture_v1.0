!                                 COPYRIGHT INFORMATION:
!************************************************************************************
!* FileCapture_v1.0.DLL - Copyright (2006) Sandia Corporation. Under the            *
!* terms of Contract DE-AC04-94AL85000, Sandia Corporation and the United States    *
!* Government retains certain rights in this software  .                            * 
!*                                                                                  *
!***********************************************************************************!

!                                        NOTICE:

! For five (5) years from February 28, 2006, the United States Government is granted for itself and 
! others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide license for this data
! to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf
! of the Government. There is provision for the possible extension of the term of this license.
! Subsequent to that period or any extension granted, the United States Government is granted for 
! itself and others action on its behalf a paid-up, nonexclusive, irrevocable worldwide license for 
! this data to reproduce, prepare derivative works, and perform publicly and display publicly, and
! to permit others to do so. The specific term of the license can be identified by inquiry made to
! Sandia Corporation or DOE.

! NEITHER THE UNITED STATES GOVERNMENT, NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR SANDIA
! CORPORATION, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES
! ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY 
! INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT
! INFRINGE PRIVATELY OWNED RIGHTS.

! Any licensee of this software has the obligation and responsibility to abide by the
! applicable export control laws, regulations, and general prohibitions relating to the
! export of technical data. Failure to obtain and export control license or other authority
! from the Government may result in criminal liability under U.S. Laws.

!                                    (End of Notice)
!   /************************************************************************
!    *                                                                      *
!    *   Patrick Mattie - SANDIA NATIONAL LABORATORIES                      *
!    *   -------------------------------------------------                  *
!    *                                                                      *
!    *                                                                      *
!    *                          FileCapture.DLL                            *
!    *                                                                      *
!    *                                                                      *
!    * DLL developed for linking with the GoldSim code to send output       *
!    * files created by external codes run in conjunction with a Goldsim    *
!    * when the model is exercised over a network using the distributed     *
!    * processing module.
!    *                                                                      *
!    ************************************************************************
!    *                                                                      *
!    *               SUBROUTINE : FCAP.f90                                  *
!    ************************************************************************/
!    *                                                                      *
!    *                                                                      *
!    *    Author(s) |Version 1.0 |   Date     | Comments                    *
!    *----------------------------------------------------------------------*
!    * P. Mattie       |  000    | 02/15/2006 | - initial version of dll    *
!    *                 |         |            |                             *
!    *                 |         |            |                             *
!    *                 |         |            |                             *
!    *                 |         |            |                             *
!    ************************************************************************/
!    *
!    ************************************************************************/
!************************************************************************************
!* FileCapture_v1.0.DLL - Copyright (2006) Sandia Corporation. Under the            *
!* terms of Contract DE-AC04-94AL85000, Sandia Corporation and the United States    *
!* Government retains certain rights in this software  .                            * 
!*                                                                                  *
!***********************************************************************************!  
!


SUBROUTINE FCAP(method, state, in, out)

  !DEC$ ATTRIBUTES DLLEXPORT,c::FCAP
  !DEC$ ATTRIBUTES value                  :: method
  !DEC$ ATTRIBUTES reference              :: state
  !DEC$ ATTRIBUTES reference              :: in
  !DEC$ ATTRIBUTES reference              :: out

! Variables

       REAL(8)                          :: in(*), out(*)
       INTEGER(4)                       :: method, state
       INTEGER                          :: fnumber, file_index, x, h
       CHARACTER                        :: in_file*36, out_file*11 
	   CHARACTER                        :: copy*25, PATH*250, FileName*25, ext*4

	   copy='                         '  ! INITIALIZE CHARACTER STRING

! Body of FCAP
!      ===========================
!      method = 0 : Initialization
!      ===========================

       IF     (method.eq.0) THEN
              continue

!      ===========================
!      method = 2 : Report version
!      ===========================

       ELSEIF (method.eq.2) THEN
              out(1) = 10  !version number of the code
              

!      =====================================
!      method = 3 : Report arguments
!                   out(1) = nber of inputs
!                   out(2) = nber of outputs 
!      =====================================

       ELSEIF (method.eq.3) THEN
              out(1) = 2
              out(2) = 1

!      ===============================
!      method = 1 : fcap
!                   Saving the results
!                   in the array 'out'
!      ===============================

       ELSEIF (method.eq.1) THEN

    OPEN (13,FILE='debug.dat',ACCESS='sequential',FORM='formatted',&
          STATUS='REPLACE')  !XXXXXX  DEBUG FILE XXXXXXXX


IF(in(1).EQ.1) THEN			! in(1) is an on/off switch 1==run DLL (network run), 0=don't run (local run)
	
	
	
			! ====================================================================== !
			! =  READ SELECTED EXTERNAL OUTPUT FILE LIST AND LOCATION FOR TRANSFER = !              
			! ====================================================================== !	

    file_index=in(2) ! in(2) from GoldSim is the realization number

    OPEN (23,FILE='fcapture.in',STATUS='OLD')
    OPEN (33,FILE='fcap.bat',ACCESS='sequential',FORM='formatted',&
          STATUS='REPLACE')

		READ(23,'(A)') PATH
		WRITE(13,'(A)') TRIM(PATH)
		WRITE(33, 2211) TRIM(PATH)
		
		READ(23,1100) fnumber
		WRITE(13,1100) fnumber
			DO a=1,fnumber
				h=1
				copy='                         '  ! INITIALIZE CHARACTER STRING
				READ(23,'(A)') FileName
				j= len_trim(FileName)
				DO k=1, j-3
					IF(FileName(h:h).EQ.'.') THEN
						DO i=0,3
						ext(i+1:i+1)=FileName(k+i:k+i)
						ENDDO					
					ELSE
					copy(h:h)=FileName(h:h)
					h=h+1
					ENDIF

				ENDDO

				WRITE(13,'(A)') TRIM(FileName)
				WRITE (33,3311) TRIM(FileName), TRIM(PATH),TRIM(copy), file_index,ext        ! Passes the output file name to be transferred

			ENDDO				


1100		FORMAT(I3)
2211		FORMAT('mkdir ','"',A, '\FCAP_FILES','"')
3311		FORMAT('copy 'A,' "',A,'\FCAP_FILES\',A,'_',I5.5,A,'"')	! for a given realization to a 
															! .dat file with the realization 
	CLOSE (UNIT=23)											! number.
	CLOSE (UNIT=33)

			! ====================================================================== !
			! =  MAKE A COPY SELECTED EXTERNAL OUTPUT FILES FOR EVERY REALIZATION  = !              
			! ====================================================================== !

    CALL SYSTEM("fcap.bat")


out(1)=1


ENDIF  !end of file capture and transfer



!      =====================
!      method = 99 : Cleanup
!      =====================

       ELSEIF (method==99) THEN

			CLOSE (UNIT=13)

		ENDIF

		RETURN

   END SUBROUTINE FCAP

