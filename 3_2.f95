!This program calculates PI using MPI in Fortran
!
!To compile: mpif90 -o <executable name> <source>
!
!To run: mpirun -n <processes> ./<executable> <number of tosses>
!


program picalc

	use mpi

        use omp_lib
	
	implicit none

	!initialize variables

	real(kind=8) :: start, finish, PI, elapsed

	real, external :: get_rand

	integer(kind=4) :: mpi_error_code, myrank, worldsize, error, j

    integer(kind=4) :: num_omp

	integer(kind=8) :: tosses, hits_in_circle, hits_in_circle_l, hits_omp ,counter, ltosses, i

	integer(kind=4) :: status(MPI_Status_Size)

    character(len=640) :: arg

	CALL MPI_Init(mpi_error_code)

	start = MPI_Wtime()

	!set variables

	call MPI_Comm_size ( MPI_COMM_WORLD, worldsize, error )

	call MPI_Comm_rank ( MPI_COMM_WORLD, myrank, error )

	hits_in_circle = 0_8

	hits_in_circle_l = 0_8

	call srand(myrank)

	!if 0, calculate total number of throws needed, broadcast that number to all processes,
	!do throws, then recieve from others in a reduce

	if(myrank == 0) then

		call getarg(1, arg)	

		read(arg, '(i100)') tosses

		ltosses = tosses / worldsize

		!brodcast total amount of work to other threads

		call MPI_Bcast(ltosses, 1, MPI_UNSIGNED_LONG_LONG, 0, MPI_COMM_WORLD, status, mpi_error_code)

		call omp_set_num_threads(8)

		!$OMP PARALLEL DO REDUCTION(+:hits_in_circle_l)

		do counter = 1, ltosses

			call toss_darts(hits_in_circle_l)

		end do
		
                !$OMP END PARALLEL DO

		call MPI_Reduce(hits_in_circle_l, hits_in_circle,1,MPI_UNSIGNED_LONG_LONG,MPI_SUM, 0,MPI_COMM_WORLD, mpi_error_code)

		PI = 4. * real(hits_in_circle) / real(tosses)

		print *, "PI is calculated as appx ", PI

		finish = MPI_Wtime()

		elapsed = finish - start

		print *, "Program ran in ", elapsed, " seconds"

		!others calculate, then send

        else

        call MPI_Bcast(ltosses, 1, MPI_UNSIGNED_LONG_LONG, 0, MPI_COMM_WORLD, status, mpi_error_code)

		if(ltosses == 0) then
		
		call exit()
		
		end if
	
		call omp_set_num_threads(8)

		!$OMP PARALLEL DO reduction(+:hits_in_circle_l)

		do counter = 1, ltosses

			call toss_darts(hits_in_circle_l)

		end do

                !$OMP END PARALLEL DO

		!send to 0

        call MPI_Reduce(hits_in_circle_l, hits_in_circle, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, 0, MPI_COMM_WORLD, mpi_error_code)

	end if

	!finalize

	CALL MPI_Finalize(mpi_error_code)

end program picalc

subroutine toss_darts(hits)

	implicit none

	real, external :: get_rand
	
	integer(kind=8) :: prev

	integer(kind=8), intent(inout) :: hits

	real(kind=8) :: x, y

	x = get_rand()

	y = get_rand()

	x = x ** 2

	y = y ** 2

	if(x + y <= 1) then

		hits = hits + 1

	end if

end subroutine toss_darts

real function get_rand()

	implicit none

	get_rand = (rand()* 2) -1

	return

end function get_rand


subroutine usage
	
	call exit()
	
end subroutine
