! written by A.Andreev 26sept18 to test matrices provided by A.Lasek (Crispin's student)
!
! this is asimple program that doe sthe following:
! 1. reads the hermitian matrix froma file
! 2. checks that it is hermitian
! 3. calls the lapack routine to find the fist 10 eigen values
!

 program test_eigen
 implicit none
 
 integer N !- size of the matrix
 
 integer, parameter :: file_pipe=239  ! in/out file pipe number
 integer, parameter :: big_char=120   ! just a big constant to fit the label of the input variant 
 
 complex, allocatable, dimension(:,:) :: Hmatr  !(N,N)- input matrix
 
 character*(big_char) :: var_label
 
 ! loops vars
 integer :: i,j, id
 
 integer :: pw_Nlev_to_calc_E=5  ! number of eigen values to do
 
 ! for eigen solver

    character*1 JOBZ, RANGE, UPLO   ! JOBZ= parameter for the PCHEEVX - determines whether to calculate eigen values or not
  real :: Emin, Emax
    
    
      INTEGER INFO, NoEigVal
      integer, allocatable, dimension(:) :: IFAIL
      COMPLEX, ALLOCATABLE, DIMENSION(:) :: WORK
      real, ALLOCATABLE, DIMENSION(:) :: RWORK
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IWORK
      COMPLEX, ALLOCATABLE, DIMENSION(:) :: ham_column
      COMPLEX, ALLOCATABLE, DIMENSION(:,:) :: Psi_QD
      real, allocatable :: Energy_Nlev_dl(:)


	  real abstol
	  
	  real slamch

       real,allocatable, dimension(:) :: vect
 
 
       real, parameter :: eps=1E-12
 
        integer, parameter :: N_print=500


!----------------------------
! this is for finding the date and time
 character(8) date_1 
 character(10) time_1
 character(5) zone_1
 integer int_time(8)
 double precision SERIAL_time_sec_before, SERIAL_time_sec_after

 
 ! 1. reading the input matrix
 
 ! allocation of the arrays
 
 ! reading the initial data
 open(file_pipe,file="input.dat" )
 read(file_pipe,*) N 
 read(file_pipe,*) var_label
 close(file_pipe)
 



 if (allocated(Hmatr)) deallocate(Hmatr)
 allocate(Hmatr(N,N))
 
 Hmatr=(0.0,0.0)
 
 ! now reading the input file
 open(file_pipe,file=trim(var_label))
 
 do i=1,N   ! reading by lines - first line, second line, etc.
   if((i/N_print)*N_print==i) print*,'reading i=',i,'of N=',N
 do j=1,N
   read(file_pipe,*) Hmatr(i,j)
   if(i>j) then
      if (cabs(Hmatr(i,j)-conjg(Hmatr(j,i)))>eps ) then
        print*,' i,j=',i,j
        print*,' Hamtr(i,j)=',Hmatr(i,j)
        print*,' Hamtr(j,i)=',Hmatr(j,i)
        print*,' difference=',cabs(Hmatr(i,j)-conjg(Hmatr(j,i)))
        print*,' eps=',eps
        stop' we terminate - matrix is non-Hermitian'
      end if  
   end if
 
 end do
 end do

 close(file_pipe)
 
 ! now calculating the eigen values...

 
   UPLO ='U' ! upper triangle of the matrix is used 
   JOBZ='V'  ! do eigenvectors 
   
   RANGE='I' ! range from IL-th to U-th eigen values   
   pw_Nlev_to_calc_E=5

   !RANGE='A' ! range from IL-th to U-th eigen values   
   !pw_Nlev_to_calc_E=N

    
   Emin=-1E10 
   Emax= 1E10 
    
    
 ALLOCATE(ham_column(1:n*(n+1)/2)) 
 
 ALLOCATE(Energy_Nlev_dl(1:n))
 
 ALLOCATE(Psi_QD(1:n,1:pw_Nlev_to_calc_E))
 
 ALLOCATE(WORK(1:2*n))
 ALLOCATE(RWORK(1:7*n))
 ALLOCATE(IWORK(1:5*n))
 ALLOCATE(IFAIL(1:n))
 
 allocate(vect(1:pw_Nlev_to_calc_E))
 
 
 ! setting the matrix
 do i=1,N
  do id=1,i
  ham_column(id + (i-1)*i/2) =Hmatr(id,i)
  end do
 end do  

    open(file_pipe,file=trim(var_label)//"_res")

 
           call DATE_AND_TIME(DATE=date_1, TIME=time_1, ZONE=zone_1, values=int_time)

           SERIAL_time_sec_before       =  int_time(4) * 3600 *24.0  &    ! days since the start of the month
                                 + int_time(5) * 3600        &    ! hours since start of the day
                                 + int_time(6) * 60          &    ! minutes since start of the hour
                                 + int_time(7)               &       
                                 + int_time(8) * 0.001

           write(file_pipe,'("Nmatr=",I6," date=",A8," time=",A10," sec=",E15.7)') N, date_1, time_1, SERIAL_time_sec_before

 
        abstol=2.0E0*slamch('S')
       CALL cHPEVX( JOBZ, RANGE, UPLO, n, ham_column, Emin, Emax, 1, pw_Nlev_to_calc_E, & 
                               abstol, NoEigVal, Energy_Nlev_dl, Psi_Qd, &
                               N, WORK, RWORK, IWORK, &
                               IFAIL, INFO )



           call DATE_AND_TIME(DATE=date_1, TIME=time_1, ZONE=zone_1, values=int_time)

           SERIAL_time_sec_after       =  int_time(4) * 3600 *24.0  &    ! days since the start of the month
                                 + int_time(5) * 3600        &    ! hours since start of the day
                                 + int_time(6) * 60          &    ! minutes since start of the hour
                                 + int_time(7)               &       
                                 + int_time(8) * 0.001


           write(file_pipe,'("Nmatr=",I6," date=",A8," time=",A10," sec=",E15.7)') N, date_1, time_1, SERIAL_time_sec_after
           write(file_pipe,'("==============")')
           write(file_pipe,'("Nmatr=",I6," time in sec=",E15.7)') N, SERIAL_time_sec_after - SERIAL_time_sec_before
 
 
 ! Now doing the result
 
 
 do i=1,pw_Nlev_to_calc_E
 
   ! checking that this is actually an eigen value
   
   vect=Matmul(Hmatr,Psi_Qd(:,i))-Energy_Nlev_dl(i)*Psi_Qd(:,i)
   
 
   write(file_pipe,*) i, Energy_Nlev_dl(i), norm2(vect) 
 
 end do
 
 close(file_pipe)
 
 end program
 
