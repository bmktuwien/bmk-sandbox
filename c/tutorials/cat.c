#include <stdio.h>
#include <stdlib.h>

void cat(FILE *fp) {
    int c;

    while ((c = getc(fp)) != EOF)
        putc(c, stdout);
}


int main(int argc, char **argv)
{
    FILE *fp;

    if (argc == 1) {
        cat(stdin);
    } else {
        while (*++argv) {
            if ((fp = fopen(*argv, "r")) == NULL) {
                fprintf(stderr, "cat: can't open %s\n", *argv);
                exit(1);
            } else {
                cat(fp);
                fclose(fp);
            }
        }
    }

    return 0;
}
