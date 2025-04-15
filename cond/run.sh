#!/bin/bash
set -x

amdflang-new -fconvert=big-endian -fPIC -fopenmp -ffree-form -O1 -g -c cpg_xxx_type_mod.F90
amdflang-new -fconvert=big-endian -fPIC -fopenmp -ffree-form -O1 -g -o master.x master.F90 cpg_xxx_type_mod.o

./master.x

