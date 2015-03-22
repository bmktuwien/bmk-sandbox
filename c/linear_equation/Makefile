##### Set exercise number #####
EXERCISE_NUMBER := 6
STUDENT_NUMBER  := 0527640
##### Set exercise number #####

# Define compilers
CC       := gcc
CFLAGS   := $(CFLAGS) -Wall -O3

STL_PATH := /.
LAPACK_HOME := /usr/local/lapack/
ATLAS_HOME := /usr/local/atlas/
PLASMA_HOME := /usr/local/plasma/install/

# Define libraries and includes
# $(LIB_HOME) is a environment variable
#LIBFLAGS  := -L$(LIB1_HOME)/lib -lnameoflib -L$(LIB2_HOME)/lib -lnameoflib ...
LIBFLAGS := -L$(PLASMA_HOME)lib -L$(ATLAS_HOME)lib -L$(LAPACK_HOME)lib -lplasma -lquark -lcoreblas -lcblas -llapacke -lpthread -latlas -llapack -lf77blas -lgfortran -lm
#INCFLAGS := -I$(LIB1_HOME)/include -I$(LIB2_HOME)/include ...
INCFLAGS := -I$(PLASMA_HOME)include -I$(ATLAS_HOME)include -I$(LAPACK_HOME)include  -I/usr/local/plasma/build/plasma_2.5.0/control -I$(STL_PATH)

SRC_FILES_C = exercise$(EXERCISE_NUMBER).c
OBJ_FILES_C = exercise$(EXERCISE_NUMBER).o


# List of files to include in the archive
TAR_FILELIST = *.c *.h *.cpp *.hpp *.sh *.plt *.pdf Makefile

# Targets
.PHONY: all clean

all: main

main: $(SRC_FILES_C)
	$(CC) -o exercise$(EXERCISE_NUMBER) $(CFLAGS) $(INCFLAGS) $^ $(LIBFLAGS)

dynlib: $(SRC_FILES_C)
	$(CC) -shared -fPIC $(CFLAGS) $(INCFLAGS) -o libexercise$(EXERCISE_NUMBER).so $^ $(LIBFLAGS)

staticlib: $(OBJ_FILES_C)
	ar rcs libexercise$(EXERCISE_NUMBER).a $(OBJ_FILES_C)


%.o: %.c
	$(CC) $(CFLAGS) $(INCFLAGS) -c -o $@ $<

run:
	./$(SOURCE_FILES)

package:
	-@mkdir -p exercise$(EXERCISE_NUMBER)/a$(STUDENT_NUMBER)/
	-@cp -u -t exercise$(EXERCISE_NUMBER)/a$(STUDENT_NUMBER)/ $(TAR_FILELIST) 2> /dev/null || :
	tar cfvz exercise$(EXERCISE_NUMBER)_a$(STUDENT_NUMBER).tar.gz exercise$(EXERCISE_NUMBER)

clean:
	-@rm -f *.o
