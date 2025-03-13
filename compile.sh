#!/bin/bash

set -xeu

module load rocm

#cleaning
./clean.sh

FC_FLAGS="-DWITH_FIAT  -I/home/grassetj/install/include -I/home/grassetj/install/include/fiat -I/home/grassetj/install/module/fiat -I/home/grassetj/install/module/parkind_dp -O2 -g -fPIC -fopenmp -c"
FC="amdflang $FC_FLAGS"


$FC factory/field_2im_factory_module.F90
$FC factory/field_3im_factory_module.F90
$FC factory/field_factory_module.F90

$FC shuffle/field_shuffle_type_module.F90
