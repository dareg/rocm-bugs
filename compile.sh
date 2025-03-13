#!/bin/bash

set -xeu

module load rocm

#cleaning
./clean.sh

FC_FLAGS="-DWITH_FIAT  -I/home/grassetj/install/include -I/home/grassetj/install/include/fiat -I/home/grassetj/install/module/fiat -I/home/grassetj/install/module/parkind_dp -O2 -g -fPIC -fopenmp -c"
FC="amdflang $FC_FLAGS"

$FC debug/field_statistics_module.F90
$FC core/field_constants_module.F90
$FC core/field_abort_module.F90
$FC core/field_defaults_module.F90
$FC core/host_alloc_module.F90
$FC core/dev_alloc_module.F90
$FC core/field_basic_module.F90

$FC core/field_2im_data_module.F90
$FC core/field_2im_module.F90
$FC core/field_3im_data_module.F90
$FC core/field_3im_module.F90

$FC factory/field_2im_factory_module.F90
$FC factory/field_3im_factory_module.F90
$FC factory/field_factory_module.F90

$FC shuffle/field_shuffle_type_module.F90
