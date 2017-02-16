# Makefile for the original form of the tra_adv benchmark.

# Compiler and associated flags must be set through the following
# environment variables:
#
#   export F90=gfortran
#   export F90FLAGS=-O3
#   export OMPFLAGS=-fopenmp
#   export MPIF90=mpif90

all: muMD.exe

FORT_FLAGS = ${F90FLAGS} ${OMPFLAGS}

KERN_OBJ = constants.o forces.o muMD.o neighbours.o parser.o potentials.o readConfig.o readControl.o readField.o sampler.o setup.o types.o units.o useful.o

forces.o: types.o potentials.o
potentials.o: useful.o
muMD.o: setup.o readControl.o readConfig.o readField.o neighbours.o
readControl.o: parser.o
readField.o: units.o

muMD.exe: ${KERN_OBJ}
	${F90} -o $@ ${KERN_OBJ}

%.o: %.f90
	${F90} ${FORT_FLAGS} -c $<
%.o: %.F90
	${F90} ${FORT_FLAGS} -c $<

clean:
	rm -f *.o
	rm -f *~
	rm -f *.mod
	rm -f muMD.exe

