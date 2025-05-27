#!/bin/bash
set -x
FC=/home/marguina/install/rocm-afar-7450-drop-6.0.0/bin/amdflang-new
$FC -c -fopenmp util_nsv_t_mod_prepro.F90
