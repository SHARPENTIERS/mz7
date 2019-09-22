@echo off

copy Makefile.Windows ..\Makefile
cd ..
nmake release
nmake realclean
del Makefile
cd windows
echo If all is well, the executables are in: windows\release
