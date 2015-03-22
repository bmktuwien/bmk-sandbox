#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

/* exercise 4-14 */
#define swap(t,x,y) {                           \
        t tmp = x;                              \
        x = y;                                  \
        y = tmp;                                \
    }

#define print_variable(x, t) printf(#x " = %" #t "\n", x)


/* exercise 5-1 */
int getint(int *pn)
{
    int c, sign;

    while (isspace(c = getc(stdin)));

    if (!isdigit(c) && c != '+' && c != '-' && c != EOF) {
        goto error;
    }

    sign = (c == '-') ? -1 : 1;

    if (c == '+' || c == '-')
        c = getc(stdin);

    if (!isdigit(c)) {
        goto error;
    }

    for (*pn = 0; isdigit(c); c = getc(stdin))
        *pn = 10 * *pn + c - '0';

    *pn *= sign;

    if (c != EOF)
        ungetc(c, stdin);

    return c;
 error:
    ungetc(c, stdin);
    return 0;
}


/* exercise 5-2 */
int getfloat(double *pn)
{
    int c, sign;
    double f;

    while (isspace(c = getc(stdin)));

    if (!isdigit(c) && c != '+' && c != '-' && c != EOF) {
        goto error;
    }

    sign = (c == '-') ? -1 : 1;

    if (c == '+' || c == '-')
        c = getc(stdin);

    if (!isdigit(c)) {
        goto error;
    }

    /* parse the pre decimal point portion */
    for (*pn = 0; isdigit(c); c = getc(stdin))
        *pn = 10 * *pn + c - '0';

    if (c == '.') {
        /* parse the after decimal point portion */
        c = getc(stdin);
        for (f = 10; isdigit(c); c = getc(stdin), f *= 10)
            *pn += (c - '0') / f;
    }

    *pn *= sign;

    if (c != EOF)
        ungetc(c, stdin);

    return c;

 error:
    ungetc(c, stdin);
    return 0;
}


/* exercise 5-4 */
int strend(char *s, char *t)
{
    char *p1 = s;
    char *p2 = t;

    while (*++p1);
    while (*++p2);

    while (p1 > s && p2 > t) {
        if (*p1 != *p2)
            break;

        p1--;
        p2--;
    }


    return p2 == t;
}


void reverse(char *s)
{
    char *p = s;

    while (*++p);
    p--;

    while (s < p) {
        swap(char, *s, *p);
        s++;
        p--;
    }
}


/* exercise 5-5 */
int mystrncpy(char *s, char *t, size_t n)
{
    char *p = t;

    while ((*s++ = *p++) && p-t < n);
    *s = '\0';

    return p - t >= n ? n : p - t - 1;
}


static int daytab[2][13] = {
    {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
    {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
};


/* exercise 5-8 */
int day_of_year(int year, int month, int day)
{
    int leap, i;

    if (year < 0 || month > 12 || month < 1)
        return -1;

    leap = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;

    if (day < 1 || day > daytab[leap][month])
        return -1;

    for (i = 1; i < month; i++) {
        day += daytab[leap][i];
    }

    return day;
}


/* exercise 5-8 */
int month_day(int year, int n, int *month, int *day)
{
    int leap, i;

    if (year < 0)
        return -1;

    leap = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;

    if (n < 1 || n > 365 + leap)
        return -1;


    for (i = 1; n - daytab[leap][i] > 0; i++) {
        n -= daytab[leap][i];
    }

    *day = n;
    *month = i;

    return 1;
}


int main(int argc, char **argv)
{
    int day = day_of_year(2014, 12, 21);
    int month, day2;
    month_day(2014, day, &month, &day2);
    print_variable(day, d);
    print_variable(month, d);
    print_variable(day2, d);

    return 0;
}
