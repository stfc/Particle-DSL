# Makefile for manual, PSyKAl version of microMD.

# Compiler and associated flags must be set through the following
# environment variables:
#
#   export F90=gfortran
#   export F90FLAGS=-O3

all: muMD.exe

FORT_FLAGS = ${F90FLAGS} ${OMPFLAGS}

INFRA_OBJ = constants.o neighbours.o parser.o potentials.o readConfig.o readControl.o readField.o sampler.o setup.o types.o units.o useful.o
KERN_OBJ = forces.o verlet2_kernel.o force_kernel.o

${KERN_OBJ}: ${INFRA_OBJ}

forces.o: types.o potentials.o
potentials.o: useful.o
muMD.o: setup.o readControl.o readConfig.o readField.o neighbours.o psy.o
readControl.o: parser.o
readField.o: units.o
psy.o: ${KERN_OBJ}
setup.o: constants.o types.o useful.o

muMD.exe: muMD.o psy.o ${KERN_OBJ}
	${F90} -o $@ muMD.o psy.o ${KERN_OBJ} ${INFRA_OBJ}

%.o: %.f90
	${F90} ${FORT_FLAGS} -c $<
%.o: %.F90
	${F90} ${FORT_FLAGS} -c $<

clean:
	rm -f *.o
	rm -f *~
	rm -f *.mod
	rm -f muMD.exe

