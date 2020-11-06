@ECHO OFF
setlocal enabledelayedexpansion
REM Batch script used to test a given module

REM Read input data
set compiler=%1
set module=%2

REM Test the given module
cd build
%compiler% ..\src\test_%module%.f90 >tmp.log 2>&1
IF %ERRORLEVEL% NEQ 0 (
  echo %module%, compilation : FAIL (see %module%_comp.log^)
  move tmp.log %module%_comp.log >nul
) ELSE (
    echo %module%, compilation : PASS
    start /wait cmd /c test_%module%.exe ^>tmp.log ^2^>^&^1
    IF !ERRORLEVEL! NEQ 0 (
        move tmp.log %module%.log  >nul
        echo %module%, unit testing: FAIL (see %module%.log^)
    ) ELSE (
        echo %module%, unit testing: PASS
    )
)

cd ..