@ECHO OFF
REM Batch script used to test all modules

REM Read input data
set compiler=ifort
IF "%1" NEQ "" (
    set compiler=%1
)
echo %compiler% is used

call test_module.bat %compiler% sort_mod

call test_module.bat %compiler% find_mod

call test_module.bat %compiler% resize_array_mod

call test_module.bat %compiler% linalg_mod
