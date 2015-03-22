##### Set exercise number #####
EXERCISE_NUMBER := 4
STUDENT_NUMBER  := 0123456
##### Set exercise number #####

# Define compilers
CC       ?= gcc
CFLAGS   := $(CFLAGS) -Wall -O2 -march=native -mfpmath=sse -funroll-loops

all: exercise4

exercise4: exercise4.o
	$(CC) $(CFLAGS) -o exercise4 exercise4.o

exercise4.o: src/exercise4.c
	$(CC) $(CFLAGS) -c src/exercise4.c

package:
	-@mkdir -p exercise$(EXERCISE_NUMBER)/a$(STUDENT_NUMBER)/
	-@cp -u -t exercise$(EXERCISE_NUMBER)/a$(STUDENT_NUMBER)/ src/exercise4.c 2> /dev/null || :
	tar cfvz exercise$(EXERCISE_NUMBER)_a$(STUDENT_NUMBER).tar.gz exercise$(EXERCISE_NUMBER)

clean:
	-@rm -f *.o
