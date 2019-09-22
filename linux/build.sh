#!/bin/sh

cp Makefile.Linux ../Makefile
cd ..
make release
make realclean
rm Makefile
echo If all is well, the executables are in: linux/release
