PROGRAM LOG_NORMALIZE
    IMPLICIT NONE

    INTEGER, PARAMETER :: MAX_N = 500000

    CHARACTER(LEN=512) :: LINE
    CHARACTER(LEN=512) :: INPUT_FILE
    CHARACTER(LEN=512) :: OUTPUT_FILE
    CHARACTER(LEN=512) :: CLOSE_STR

    REAL(8) :: CLOSE_PRICE(MAX_N)
    REAL(8) :: LOG_RETURN(MAX_N)
    REAL(8) :: NORM_RETURN(MAX_N)
    REAL(8) :: PREV_CLOSE
    REAL(8) :: CURRENT_CLOSE
    REAL(8) :: LOGV
    REAL(8) :: MEAN_RET
    REAL(8) :: STD_RET
    REAL(8) :: SUM_RET
    REAL(8) :: SUM_SQ

    INTEGER :: IOS
    INTEGER :: OUT_IDX
    INTEGER :: FILE_COUNT
    INTEGER :: P1, P2, P3, P4, P5
    INTEGER :: READ_IOS
    INTEGER :: I

    LOGICAL :: FIRST_PRICE

    OUTPUT_FILE = "output/normalized_returns.csv"

    CALL EXECUTE_COMMAND_LINE('find data/raw -type f -name "*.csv" | sort > file_list.txt')

    OPEN(UNIT=30, FILE="file_list.txt", STATUS="OLD", ACTION="READ", IOSTAT=IOS)

    IF (IOS /= 0) THEN
        PRINT *, "Could not create/read file_list.txt"
        PRINT *, "Make sure folder data/raw exists."
        STOP
    END IF

    FIRST_PRICE = .TRUE.
    OUT_IDX = 0
    FILE_COUNT = 0

    DO
        READ(30, '(A)', IOSTAT=IOS) INPUT_FILE
        IF (IOS /= 0) EXIT

        IF (LEN_TRIM(INPUT_FILE) == 0) CYCLE

        FILE_COUNT = FILE_COUNT + 1
        PRINT *, "Reading: ", TRIM(INPUT_FILE)

        OPEN(UNIT=10, FILE=TRIM(INPUT_FILE), STATUS="OLD", ACTION="READ", IOSTAT=IOS)

        IF (IOS /= 0) THEN
            PRINT *, "Could not open file: ", TRIM(INPUT_FILE)
            CYCLE
        END IF

        DO
            READ(10, '(A)', IOSTAT=IOS) LINE
            IF (IOS /= 0) EXIT

            IF (LEN_TRIM(LINE) == 0) CYCLE

            P1 = INDEX(LINE, ",")
            IF (P1 == 0) CYCLE

            P2 = INDEX(LINE(P1+1:), ",")
            IF (P2 == 0) CYCLE
            P2 = P2 + P1

            P3 = INDEX(LINE(P2+1:), ",")
            IF (P3 == 0) CYCLE
            P3 = P3 + P2

            P4 = INDEX(LINE(P3+1:), ",")
            IF (P4 == 0) CYCLE
            P4 = P4 + P3

            P5 = INDEX(LINE(P4+1:), ",")
            IF (P5 == 0) CYCLE
            P5 = P5 + P4

            CLOSE_STR = LINE(P4+1:P5-1)

            READ(CLOSE_STR, *, IOSTAT=READ_IOS) CURRENT_CLOSE
            IF (READ_IOS /= 0) CYCLE

            IF (CURRENT_CLOSE <= 0.0D0) CYCLE

            IF (FIRST_PRICE) THEN
                PREV_CLOSE = CURRENT_CLOSE
                FIRST_PRICE = .FALSE.
                CYCLE
            END IF

            LOGV = LOG(CURRENT_CLOSE / PREV_CLOSE)

            OUT_IDX = OUT_IDX + 1

            IF (OUT_IDX > MAX_N) THEN
                PRINT *, "Too many rows. Increase MAX_N."
                STOP
            END IF

            CLOSE_PRICE(OUT_IDX) = CURRENT_CLOSE
            LOG_RETURN(OUT_IDX) = LOGV

            PREV_CLOSE = CURRENT_CLOSE
        END DO

        CLOSE(10)
    END DO

    CLOSE(30)

    IF (FILE_COUNT == 0) THEN
        PRINT *, "No CSV files found in data/raw"
        STOP
    END IF

    IF (OUT_IDX == 0) THEN
        PRINT *, "CSV files found, but no valid close prices were read."
        PRINT *, "This program expects close price in column 5."
        STOP
    END IF

    SUM_RET = 0.0D0

    DO I = 1, OUT_IDX
        SUM_RET = SUM_RET + LOG_RETURN(I)
    END DO

    MEAN_RET = SUM_RET / DBLE(OUT_IDX)

    SUM_SQ = 0.0D0

    DO I = 1, OUT_IDX
        SUM_SQ = SUM_SQ + (LOG_RETURN(I) - MEAN_RET) ** 2
    END DO

    IF (OUT_IDX > 1) THEN
        STD_RET = SQRT(SUM_SQ / DBLE(OUT_IDX - 1))
    ELSE
        STD_RET = 1.0D0
    END IF

    IF (STD_RET == 0.0D0) STD_RET = 1.0D0

    DO I = 1, OUT_IDX
        NORM_RETURN(I) = (LOG_RETURN(I) - MEAN_RET) / STD_RET
    END DO

    OPEN(UNIT=20, FILE=OUTPUT_FILE, STATUS="REPLACE", ACTION="WRITE", IOSTAT=IOS)

    IF (IOS /= 0) THEN
        PRINT *, "Could not create output file."
        STOP
    END IF

    WRITE(20, '(A)') "INDEX,CLOSE,LOGRETURN,NORMALIZED_RETURN"

    DO I = 1, OUT_IDX
        WRITE(20, '(I0,",",F12.5,",",F14.8,",",F14.8)') &
            I, CLOSE_PRICE(I), LOG_RETURN(I), NORM_RETURN(I)
    END DO

    CLOSE(20)

    PRINT *, "Created: ", TRIM(OUTPUT_FILE)
    PRINT *, "CSV files used: ", FILE_COUNT
    PRINT *, "Rows written: ", OUT_IDX
    PRINT *, "Mean log return: ", MEAN_RET
    PRINT *, "Std log return: ", STD_RET

END PROGRAM LOG_NORMALIZE