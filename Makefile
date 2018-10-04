F90= mpif90
F90_serial= mpif90

#setting the flags
# this is appended to the end of the exe file
EXE_TAG=_1
Flag_OpenMP=openmp
Flag_MPI=mpi
Flag_Optimisation=-O3
Flag_DEBUG= -check all -debug -traceback -warn interface

F90FLAGS= $(Flag_Optimisation) -$(Flag_OpenMP) $(Flag_DEBUG) -D$(Flag_OpenMP) -D$(Flag_MPI) -fpp
F90FLAGS_serial=  $(Flag_Optimisation) -$(Flag_OpenMP) $(Flag_DEBUG) -D$(Flag_OpenMP) -fpp
#F90FLAGS_serial= -debug -check bounds -O0 -traceback $(Flag_OpenMP) #-i8
#
LD= mpif90
LD_serial= mpif90
#
#=======================================================================================================
#  flags on aa-i3 : Intel Mi-3 CPU with 4Gb memory 
#   the same is also on sapphire 
#
LDFLAGS=  $(Flag_Optimisation) -$(Flag_OpenMP)  -D$(Flag_OpenMP) -L/opt/intel/Compiler/11.1/072/mkl/lib/em64t -Wl,--start-group -lmkl_scalapack_lp64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_openmpi_lp64 -Wl,--end-group 
#
#
LDFLAGS_serial= $(Flag_Optimisation) -$(Flag_OpenMP) -Wl,--start-group -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -Wl,--end-group -lpthread
#
#
#--------------------------------------------------------------------------
#
#  Source  = test
#
SOURCE_test = test_eigen_v2.f90
#=====================================================
#   PROJECTS
#----------------------------------

#---------------------------------------------------------------------------------------
# Serial project with all tasks - including NMR

OBJ_SOURCE_test = $(patsubst %.f90,%.o,$(SOURCE_test))

$(OBJ_SOURCE_test): %.o : %.f90 
	$(F90_serial) $(F90FLAGS_serial) -c $<

EXE_test = test

$(EXE_test): $(OBJ_SOURCE_test)
	$(LD_serial) $(LDFLAGS_serial) $^ -o $@_$(Flag_OpenMP)$(Flag_Optimisation)$(EXE_TAG)

#---------------------------------------------------------------------------------------
# .PHONY: clean
# 
# clean:
# 	rm -f *.o *.mod $(EXE_spE_cc1) $(EXE_DOS_cc1) 


