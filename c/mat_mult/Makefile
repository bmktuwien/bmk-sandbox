##### Set exercise number #####
EXERCISE_NUMBER := 2
STUDENT_NUMBER  := 0123456
##### Set exercise number #####

# Define compilers
CC       ?= gcc
CFLAGS   := $(CFLAGS) -Wall -O3 -march=native -mfpmath=sse -funroll-loops -ffast-math

all: exercise2

exercise2: exercise2.o
	$(CC) $(CFLAGS) -o exercise2 exercise2.o

exercise2.o: src/exercise2.c
	$(CC) $(CFLAGS) -c src/exercise2.c

package:
	-@mkdir -p exercise$(EXERCISE_NUMBER)/a$(STUDENT_NUMBER)/
	-@cp -u -t exercise$(EXERCISE_NUMBER)/a$(STUDENT_NUMBER)/ src/exercise2.c 2> /dev/null || :
	tar cfvz exercise$(EXERCISE_NUMBER)_a$(STUDENT_NUMBER).tar.gz exercise$(EXERCISE_NUMBER)

clean:
	-@rm -f *.o
