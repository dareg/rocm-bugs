#!/bin/bash

set -x

amdflang-new -I. -S -fconvert=big-endian -fPIC -fopenmp -ffree-form -O1 toto.F90
amdflang-new -I. -c -fconvert=big-endian -fPIC -fopenmp -ffree-form -O1 toto.F90

cat toto.s

