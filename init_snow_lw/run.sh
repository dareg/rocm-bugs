#!/bin/bash
set -x

amdflang-new -c -fconvert=big-endian -fPIC -fopenmp -ffree-form -O1 -g -fdefault-real-8 init_snow_lw.F90
