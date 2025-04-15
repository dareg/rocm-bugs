#!/bin/bash
set -x
amdflang-new -c -fconvert=big-endian \
  -fPIC -fopenmp -ffree-form -O1 -g \
  toto.F90
