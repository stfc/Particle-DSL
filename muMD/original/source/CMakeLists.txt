
set(microMD_SRCS 
muMD.F90
useful.F90
types.F90
parser.F90
readControl.F90
constants.F90
readConfig.F90
readField.F90
setup.F90
potentials.F90
neighbours.F90
forces.F90
sampler.F90
units.F90
)
add_executable(${target_name} ${microMD_SRCS} )
set_target_properties(${target_name} PROPERTIES LINK_FLAGS "${OMP_LINK_FLAGS}")
## sets the linking
link_directories (${LIBRARY_OUTPUT_PATH})
target_link_libraries(${target_name} ${LIBS})

