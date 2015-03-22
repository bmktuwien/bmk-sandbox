/**
 * Grammar specification
 *
 * dcl:  optional *'s direct-dcl
 * direct-dcl: name
 *             (dcl)
 *             direct-dcl()
 *             direct-dcl[optional-size]
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAXTOKEN  10000
#define MAXOUTPUT 10000

#define get_ident() get_while(isalnum);
#define get_num() get_while(isdigit);
#define getc_(c) {                              \
        skip_whitespace();                      \
        c = getc(stdin);                        \
    }

/* ---------------------------------------------------------------------------*/

typedef enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    ARRAY,
    FUNCTION,
    IDENT,
    END
} token_type_t;


typedef struct {
    token_type_t type;
    char *val;
} token_t;


/* global storage allocation */
char output[MAXOUTPUT];
char token_val[MAXTOKEN];
token_t token = {
    END,
    token_val
};


void skip_whitespace()
{
    int c;

    while(isspace((c = getc(stdin))));
    ungetc(c, stdin);
}

char *get_while(int (*fp)(int))
{
    int c, i=0;

    while ((c = getc(stdin)) != EOF && (*fp)(c)) {
        token.val[i] = c;
        i++;
    }

    if (c != EOF)
        ungetc(c, stdin);

    token.val[i] = '\0';
}


token_t gettoken()
{
    int c;

    getc_(c);

    if (c == '(') {
        c = getc(stdin);
        if (c == ')') {
            token.type = FUNCTION;
        } else {
            ungetc(c, stdin);
            token.type = LEFT_PAREN;
        }
    } else if (isalpha(c)) {
        ungetc(c, stdin);
        token.type = IDENT;
        get_ident();
    } else if (c == ')') {
        token.type = RIGHT_PAREN;
    } else if (c == '[') {
        token.type = ARRAY;
        token.val[0] = '\0';
        skip_whitespace();
        getc_(c);

        if (isdigit(c)) {
            ungetc(c, stdin);
            get_num();
            getc_(c);
        }
        if (c != ']') {
            printf("error: expected ']'\n");
            exit(1);
        }
    } else if (c == EOF) {
        token.type = END;
    } else {
        printf("error: unknown symbol: %c\n", c);
        exit(1);
    }

    return token;
}

void direct_dcl();

void dcl() {
    int c, n=0;

    skip_whitespace();
    while((c = getc(stdin)) == '*') {
        skip_whitespace();
        n++;
    }
    ungetc(c, stdin);

    direct_dcl();

    while (n--)
        strcat(output, " pointer to");
}


void direct_dcl() {
    char buf[1024];

    gettoken();
    if (token.type == END)
        return;

    if (token.type == LEFT_PAREN) {
        dcl();
        if (token.type != RIGHT_PAREN) {
            printf("error: expected ')'\n");
            exit(1);
        }
    } else if (token.type == IDENT) {
        sprintf(buf, " %s is", token.val);
        strcat(output, buf);
    } else {
        printf("error: malformed declaration!\n");
        exit(1);
    }

    while ((gettoken()).type == ARRAY || token.type == FUNCTION) {
        if (token.type == ARRAY) {
            sprintf(buf, " array[%s] of", token.val);
            strcat(output, buf);
        } else if (token.type == FUNCTION) {
            sprintf(buf, " function returning");
            strcat(output, buf);
        }
    }
}


int main(int argc, char **argv)
{
    output[0] = '\0';
    char type[30];
    get_ident();
    strcpy(type, token.val);

    dcl();

    printf("converted: %s %s\n", output, type);
    return 0;
}
