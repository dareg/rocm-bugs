#!/bin/bash

set -xeu

module load rocm

#cleaning
./clean.sh

FC_FLAGS="-c"
FC="amdflang $FC_FLAGS"

$FC factory/field_2im_factory_module.F90
$FC factory/field_3im_factory_module.F90
$FC factory/field_factory_module.F90
$FC shuffle/field_shuffle_type_module.F90
