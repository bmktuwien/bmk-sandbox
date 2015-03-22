#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAXLINE 1024

#define fopen_error(prog, fn) {                                 \
        fprintf(stderr, "%s: can't open %s\n", prog, fn);       \
        exit(1);                                                \
    }                                                           \


int my_getline(FILE *fp, char *line, int max)
{
    if (fgets(line, max, fp) == NULL)
        return 0;
    else
        return strlen(line);
}

void diff(FILE *fp1, FILE *fp2)
{
    int c1, c2;
    char l1[MAXLINE], l2[MAXLINE];

    do {
        c1 = my_getline(fp1, l1, MAXLINE);
        c2 = my_getline(fp2, l2, MAXLINE);

        if (!c1)
            l1[0] = '\0';

        if (!c2)
            l2[0] = '\0';

        if (strcmp(l1, l2) != 0) {
            printf("file1: %s\n", l1);
            printf("file2: %s\n", l2);
            break;
        }
    } while(c1 != 0 && c2 != 0);


}

int main(int argc, char **argv)
{
    char *prog = argv[0];
    FILE *fp1, *fp2;

    if (argc != 3) {
        fprintf(stderr, "Usage: diff <file1> <file2>");
    } else {
        if ((fp1 = fopen(argv[1], "r")) == NULL)
            fopen_error(prog, argv[1]);

        if ((fp2 = fopen(argv[2], "r")) == NULL)
            fopen_error(prog, argv[2]);

        diff(fp1, fp2);

        fclose(fp1);
        fclose(fp2);
    }
}
