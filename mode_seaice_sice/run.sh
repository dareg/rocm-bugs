#!/bin/bash
set -x
amdflang-new -c -fconvert=big-endian -fPIC -fopenmp -ffree-form -g -fdefault-real-8 mode_seaice_sice.F90
