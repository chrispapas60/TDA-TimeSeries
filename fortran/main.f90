PROGRAM COMPUTE_LOG_RETURNS
    IMPLICIT NONE

    INTEGER, PARAMETER :: MAX_N = 20000

    CHARACTER(LEN=256) :: LINE
    CHARACTER(LEN=256) :: INPUT_FILE
    CHARACTER(LEN=256) :: OUTPUT_FILE

    REAL(8) :: CLOSE_PRICE(MAX_N)
    REAL(8) :: LOG_RETURN(MAX_N)

    INTEGER :: N, I, IOS
    INTEGER :: YEAR, MONTH
    LOGICAL :: FILE_EXISTS

    OUTPUT_FILE = "output/returns.csv"
    N = 0

    DO YEAR = 2018, 2026
        DO MONTH = 1, 12

            WRITE(INPUT_FILE, '("data/raw/BTCTUSD-1d-",I4.4,"-",I2.2,".csv")') YEAR, MONTH

            INQUIRE(FILE=INPUT_FILE, EXIST=FILE_EXISTS)

            IF (.NOT. FILE_EXISTS) CYCLE

            OPEN(UNIT=10, FILE=INPUT_FILE, STATUS="old", ACTION="read", IOSTAT=IOS)

            IF (IOS /= 0) THEN
                PRINT *, "Could not open file: ", TRIM(INPUT_FILE)
                CYCLE
            END IF

            DO
                READ(10, '(A)', IOSTAT=IOS) LINE
                IF (IOS /= 0) EXIT

                N = N + 1

                IF (N > MAX_N) THEN
                    PRINT *, "Too many rows. Increase MAX_N."
                    STOP
                END IF

                CALL READ_CLOSE_FROM_LINE(LINE, CLOSE_PRICE(N))
            END DO

            CLOSE(10)

        END DO
    END DO

    IF (N < 2) THEN
        PRINT *, "Not enough close prices found."
        STOP
    END IF

    DO I = 2, N
        LOG_RETURN(I) = LOG(CLOSE_PRICE(I) / CLOSE_PRICE(I - 1))
    END DO

    OPEN(UNIT=20, FILE=OUTPUT_FILE, STATUS="replace", ACTION="write", IOSTAT=IOS)

    IF (IOS /= 0) THEN
        PRINT *, "Could not open output file."
        STOP
    END IF

    WRITE(20, '(A)') "INDEX,CLOSE,LOGRETURN"

    DO I = 2, N
        WRITE(20, '(I8,",",F18.8,",",F18.10)') I, CLOSE_PRICE(I), LOG_RETURN(I)
    END DO

    CLOSE(20)

    PRINT *, "Read close prices: ", N
    PRINT *, "Wrote returns to: ", TRIM(OUTPUT_FILE)

CONTAINS

    SUBROUTINE READ_CLOSE_FROM_LINE(LINE, CLOSE_VALUE)
        IMPLICIT NONE

        CHARACTER(LEN=*), INTENT(IN) :: LINE
        REAL(8), INTENT(OUT) :: CLOSE_VALUE

        CHARACTER(LEN=256) :: FIELDS(12)
        CHARACTER(LEN=256) :: TEMP
        INTEGER :: J, COMMA_POS

        TEMP = TRIM(LINE)

        DO J = 1, 12
            COMMA_POS = INDEX(TEMP, ",")

            IF (COMMA_POS > 0) THEN
                FIELDS(J) = TEMP(1:COMMA_POS-1)
                TEMP = TEMP(COMMA_POS+1:)
            ELSE
                FIELDS(J) = TEMP
                TEMP = ""
            END IF
        END DO

        READ(FIELDS(5), *) CLOSE_VALUE

    END SUBROUTINE READ_CLOSE_FROM_LINE

END PROGRAM COMPUTE_LOG_RETURNS
