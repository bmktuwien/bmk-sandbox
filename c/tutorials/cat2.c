#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#include <limits.h>
#include <fcntl.h>


static inline void *
ptr_align_pow2(void *ptr, size_t alignment)
{
    uintptr_t p0 = (uintptr_t) ptr;
    uintptr_t p1 = p0 + alignment - 1;

    return (void *) (p1 & (uintptr_t) ~(alignment - 1));
}

static inline void *
ptr_align(void *ptr, size_t alignment)
{
    uintptr_t p0 = (uintptr_t) ptr;
    uintptr_t p1 = p0 + alignment - 1;

    return (void *) (p1 - (p1 % alignment));
}

void simple_cat()
{
    char *buf = NULL;
    size_t buf_size;
    ssize_t n;
    struct stat fs;
    long pagesize;

    if (fstat(STDIN_FILENO, &fs) == -1) {
        fprintf(stderr, "reading file stat failed...\n");
        exit(1);
    }
    buf_size = fs.st_blksize;

    if ((pagesize = sysconf(_SC_PAGESIZE)) == -1) {
        fprintf(stderr, "Reading memory page size failed...\n");
        exit(1);
    }

    if ((buf = malloc(buf_size + pagesize - 1)) == NULL) {
        fprintf(stderr, "Allocating buffer space failed...\n");
        exit(1);
    }

    while ((n = read(STDIN_FILENO, ptr_align(buf, pagesize), buf_size))) {
        if (write(STDOUT_FILENO, buf, n) != n) {
            fprintf(stderr, "write opreation failed...\n");
            exit(1);
        }
    }

    free(buf);
}

int main(int argc, char *argv[])
{
    unsigned char uc;
    unsigned short us;

    us = 0x100 + 1;
    uc = (unsigned char) us;

    printf("uc = %d\n", uc);


    return 0;
}
