    PROGRAM SAX

    IMPLICIT NONE
    LOGICAL :: FIRST 
    INTEGER ::I , J , K, SEGLEN
    REAL(8) :: SEGSUM , MEAN ,  TEMP1, TEMP2 ,  STD, NORM
    REAL(8) , DIMENSION(486) :: SEG   
    INTEGER, PARAMETER :: MAX_N = 4000
    REAL(8), DIMENSION(MAX_N) :: SERIES
    INTEGER :: IOS , N, IDX
    REAL(8) :: X, CLOSEV , LOGV , NORMV
    CHARACTER(LEN = 100) :: HEAD
    REAL(8) , DIMENSION(486) :: NORM_SEG  
    CHARACTER(LEN = 1) :: A

    FIRST = .TRUE.

   OPEN(UNIT=67, FILE="output/normalized_returns.csv", STATUS ="OLD", ACTION = "READ", IOSTAT = IOS)

   IF (IOS /= 0) THEN
	PRINT*,"COULD NOT OPEN FILE"
	STOP
   END IF
   
   N = 0
	
   READ(67, '(A)', IOSTAT=IOS) HEAD
   DO  

	READ(67,*,IOSTAT=IOS) IDX , CLOSEV , LOGV , NORMV
	IF (IOS/=0) EXIT
	
	IF (FIRST) THEN
	FIRST = .FALSE.
	CYCLE
	END IF	

	N = N + 1
	IF (N > MAX_N) THEN 
	   PRINT*, "TO MANY VALUES, VALUE LIMIT IS SET TO 4000"
	   STOP
	END IF
	SERIES(N) = NORMV
   END DO
   CLOSE(67)
   PRINT*, "LOADED", N , "VALUES"
   PRINT*, SERIES(1)
    

    K = 0

    DO I = 1 , N 
	SEGSUM = SEGSUM + SERIES(I)    
        IF (MOD(I , 5 ) == 0) THEN 
	K = K + 1
	SEG(K) = SEGSUM / 5.000
	SEGSUM = 0.000
	END IF
    END DO

    
    MEAN = 0.0D0
    DO I = 1 , 486 
	MEAN = (SEG(I) + SEGSUM) / 486

    END DO
    
    TEMP2 = 0.00

    DO I = 1, 486
	TEMP1 = (SEG(I) - MEAN) ** 2
	TEMP2 = TEMP2 + TEMP1
	TEMP1 = 0 
    END DO 
    
    STD = SQRT(TEMP2 / 485.0)
    
    DO I = 1 , 486 
	NORM_SEG(I) = (SEG(I) -  MEAN) / STD
 
    END DO
    DO I = 1, 486	
        PRINT*, NORM_SEG(I)
    END DO
    
    

    OPEN(UNIT = 69 , FILE = "output/sax_results.csv" , STATUS = "REPLACE" , ACTION = "WRITE")

    WRITE(69 , '(A)') "N , NORM_VALUES , SAX_SYMBOL"



    DO I = 1, 486 
    
	IF(NORM_SEG(I) < -0.84D0) THEN 
		 A = "A"
	ELSE IF(NORM_SEG(I) < -0.25D0) THEN
		 A = "B"
	ELSE IF (NORM < 0.25D0) THEN 
		 A = "C"
	ELSE IF( NORM_SEG(I) < 0.84D0) THEN
		 A ="D"
	ELSE
		 A = "E"
	END IF
	


    WRITE(69 , '(I0 ,"," ,F15.8 ,",", A)') &
	 I , NORM_SEG(I) , A

    END DO

   END PROGRAM SAX

 
