echo %0
rem Delphi post build commands:
rem set PROJECTNAME=$(PROJECTNAME)        --> set environment to actual projectname
rem set DOXYPROJECTNAME=$(OUTPUTFILENAME) --> set environment to actual project-outputfile
rem cd .\Doxygen                          --> changes directory: path to this file (Doxygen.bat)
rem Doxygen.bat                           --> execute this file

IF EXIST MakeDoxyDok.bat MakeDoxyDok

rem ----------------------------------------------------------------------------

echo ---------------------------------------------------------------------------
echo Creating Doxygen documentation for %PROJECTNAME%
echo ---------------------------------------------------------------------------

rem set path to Doxygen executable:
set DOXYGEN_PATH=C:\Anwendungen\DocTools\doxygen\bin

rem Set paths to doxygen documentation directory (where Doxyfile is located):
set DOXYDOK_PATH=..\..\Doxygen
set DOXYFILE=Doxyfile

rem switch to doxygen documentation directory
rem and create doxygen documentation
cd %DOXYDOK_PATH%
%DOXYGEN_PATH%\doxygen.exe %DOXYFILE%

