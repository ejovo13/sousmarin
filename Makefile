INC = -I/home/ejovo/.r/Rcpp/include -I/usr/include/R
CC = g++

compile: test.c
	$(CC) test.c -o t $(INC)

fort: test.f90
	gfortran -fpic -shared test.f90 -o test.so