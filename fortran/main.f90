program compute_log_returns
    implicit none

    integer, parameter :: max_n = 20000

    character(len=256) :: line
    character(len=256) :: input_file
    character(len=256) :: output_file

    real(8) :: close_price(max_n)
    real(8) :: log_return(max_n)

    integer :: n, i, ios
    integer :: year, month
    logical :: file_exists

    output_file = "output/returns.csv"
    n = 0

    do year = 2018, 2026
        do month = 1, 12

            write(input_file, '("data/raw/BTCTUSD-1d-",I4.4,"-",I2.2,".csv")') year, month

            inquire(file=input_file, exist=file_exists)

            if (.not. file_exists) cycle

            open(unit=10, file=input_file, status="old", action="read", iostat=ios)

            if (ios /= 0) then
                print *, "Could not open file: ", trim(input_file)
                cycle
            end if

            do
                read(10, '(A)', iostat=ios) line
                if (ios /= 0) exit

                n = n + 1

                if (n > max_n) then
                    print *, "Too many rows. Increase max_n."
                    stop
                end if

                call read_close_from_line(line, close_price(n))
            end do

            close(10)

        end do
    end do

    if (n < 2) then
        print *, "Not enough close prices found."
        stop
    end if

    do i = 2, n
        log_return(i) = log(close_price(i) / close_price(i - 1))
    end do

    open(unit=20, file=output_file, status="replace", action="write", iostat=ios)

    if (ios /= 0) then
        print *, "Could not open output file."
        stop
    end if

    write(20, '(A)') "Index,Close,LogReturn"

    do i = 2, n
        write(20, '(I8,",",F18.8,",",F18.10)') i, close_price(i), log_return(i)
    end do

    close(20)

    print *, "Read close prices: ", n
    print *, "Wrote returns to: ", trim(output_file)

contains

    subroutine read_close_from_line(line, close_value)
        implicit none

        character(len=*), intent(in) :: line
        real(8), intent(out) :: close_value

        character(len=256) :: fields(12)
        character(len=256) :: temp
        integer :: j, comma_pos

        temp = trim(line)

        do j = 1, 12
            comma_pos = index(temp, ",")

            if (comma_pos > 0) then
                fields(j) = temp(1:comma_pos-1)
                temp = temp(comma_pos+1:)
            else
                fields(j) = temp
                temp = ""
            end if
        end do

        ! Binance kline CSV:
        ! column 5 = close price
        read(fields(5), *) close_value

    end subroutine read_close_from_line

end program compute_log_returns
