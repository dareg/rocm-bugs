#!/bin/bash

set -xeu

module load rocm

FC_FLAGS="-DWITH_FIAT  -I/home/grassetj/install/include -I/home/grassetj/install/include/fiat -I/home/grassetj/install/module/fiat -I/home/grassetj/install/module/parkind_dp -O2 -g -fPIC -fopenmp -c"
FC="amdflang $FC_FLAGS"

$FC debug/field_statistics_module.F90
$FC core/field_constants_module.F90
$FC core/field_abort_module.F90
$FC core/field_defaults_module.F90
$FC core/host_alloc_module.F90
$FC core/dev_alloc_module.F90
$FC core/field_basic_module.F90

$FC core/field_1im_data_module.F90
$FC core/field_1im_module.F90
$FC core/field_1lm_data_module.F90
$FC core/field_1lm_module.F90
$FC core/field_1rd_data_module.F90
$FC core/field_1rd_module.F90
$FC core/field_1rm_data_module.F90
$FC core/field_1rm_module.F90
$FC core/field_2im_data_module.F90
$FC core/field_2im_module.F90
$FC core/field_2lm_data_module.F90
$FC core/field_2lm_module.F90
$FC core/field_2rd_data_module.F90
$FC core/field_2rd_module.F90
$FC core/field_2rm_data_module.F90
$FC core/field_2rm_module.F90
$FC core/field_3im_data_module.F90
$FC core/field_3im_module.F90
$FC core/field_3lm_data_module.F90
$FC core/field_3lm_module.F90
$FC core/field_3rd_data_module.F90
$FC core/field_3rd_module.F90
$FC core/field_3rm_data_module.F90
$FC core/field_3rm_module.F90
$FC core/field_4im_data_module.F90
$FC core/field_4im_module.F90
$FC core/field_4lm_data_module.F90
$FC core/field_4lm_module.F90
$FC core/field_4rd_data_module.F90
$FC core/field_4rd_module.F90
$FC core/field_4rm_data_module.F90
$FC core/field_4rm_module.F90
$FC core/field_5im_data_module.F90
$FC core/field_5im_module.F90
$FC core/field_5lm_data_module.F90
$FC core/field_5lm_module.F90
$FC core/field_5rd_data_module.F90
$FC core/field_5rd_module.F90
$FC core/field_5rm_data_module.F90
$FC core/field_5rm_module.F90
$FC core/field_async_module.F90
$FC core/field_module.F90

$FC factory/field_3lm_factory_module.F90
$FC factory/field_2rm_factory_module.F90
$FC factory/field_2im_factory_module.F90
$FC factory/field_1im_factory_module.F90
$FC factory/field_1rd_factory_module.F90
$FC factory/field_5rd_factory_module.F90
$FC factory/field_5lm_factory_module.F90
$FC factory/field_4rd_factory_module.F90
$FC factory/field_2lm_factory_module.F90
$FC factory/field_1lm_factory_module.F90
$FC factory/field_5im_factory_module.F90
$FC factory/field_4im_factory_module.F90
$FC factory/field_2rd_factory_module.F90
$FC factory/field_3rd_factory_module.F90
$FC factory/field_3rm_factory_module.F90
$FC factory/field_3im_factory_module.F90
$FC factory/field_5rm_factory_module.F90
$FC factory/field_1rm_factory_module.F90
$FC factory/field_4lm_factory_module.F90
$FC factory/field_4rm_factory_module.F90
$FC factory/field_factory_module.F90

$FC util/field_2im_access_module.F90
$FC util/field_3im_access_module.F90
$FC util/field_access_module.F90

$FC shuffle/field_2im_shuffle_module.F90
$FC shuffle/field_shuffle_type_module.F90
