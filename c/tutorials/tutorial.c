#include <stdio.h>
#include <stdlib.h>

void bailout(char *msg)
{
    fprintf(stderr, "bailout: %s", msg);
    exit(EXIT_FAILURE);
}

void tutorial4()
{
    int counters[256];

    int c = 0;

    //intialize counter variables
    while (c < 256) {
        counters[c] = 0;
        c++;
    }

    //set counters
    while ((c = getchar()) != EOF) {
        if (c < 0 || c > 255) {
            bailout("not allowed character");
        }
        counters[c]++;
    }

    //print histogram
    c = 0;
    while (c < 256) {
        if (counters[c] > 0) {
            printf("%c: ", (char) c);

            int i = 0;
            while (i < counters[c]) {
                putchar('#');
                i++;
            }

            putchar('\n');
        }
        c++;
    }
}

void tutorial3()
{
    /* make the tab, newline visible */
    int c;

    while ((c = getchar()) != EOF) {
        if (c == '\t') {
            putchar('\\');
            putchar('t');
        } else if (c == '\n') {
            putchar('\\');
            putchar('n');
        } else {
            putchar(c);
        }
    }
}

void tutorial2()
{
    int c;
    char flag = 0;

    while ((c = getchar()) != EOF) {
        if (!flag && c == ' ') {
            flag = 1;
            putchar(c);
        } else if (flag && c != ' ') {
            flag = 0;
            putchar(c);
        } else if (!flag && c != ' ') {
            putchar(c);
        }
    }
}

void tutorial1()
{
    float fahr, celcius;
    float lower, upper, step;

    lower = 0.0;
    step = 20.0;
    upper = 300.0;

    printf("Fahrenheit - Celsius\n");
    fahr = lower;
    while (fahr <= upper) {
        celcius = 5.0 * (fahr - 32.0) / 9.0;
        printf("%8.2f - %5.2f\n", fahr, celcius);
        fahr += step;
    }

    printf("\n\n\n");


    celcius = lower;
    while (celcius <= upper) {
        fahr = celcius * (9.0/5.0) + 32.0;
        printf("%8.2f - %5.2f\n", fahr, celcius);
        celcius += step;
    }
}

int main()
{
    tutorial4();

    return 0;
}
