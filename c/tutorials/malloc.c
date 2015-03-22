#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

typedef long Align;

union header {
    struct {
        union header *ptr;
        unsigned size;
    } s;

    Align x;
};

typedef union header Header;


static Header base;
static Header *freep = NULL;

#define NALLOC 1024 /* minimum units to request */

void myfree(void *ap)
{
    Header *p, *bp;

    bp = (Header *)ap - 1;
    for (p = freep; !(p < bp && p->s.ptr > bp); p = p->s.ptr)
        if (p >= p->s.ptr && (bp > p || bp < p->s.ptr))
            break; /* is at the beginning or end of the arena */

    if (bp + bp->s.size == p->s.ptr) {
        bp->s.size = p->s.ptr->s.size;
        bp->s.ptr = p->s.ptr->s.ptr;
    } else
        bp->s.ptr = p->s.ptr;

    if (p + p->s.size == bp) {
        /* merge left */
        p->s.size += bp->s.size;
        p->s.ptr = bp->s.ptr;
    } else
        p->s.ptr = bp;

    freep = p;

}


Header *morecore(unsigned nunits)
{
    Header *p;
    char *cp;

    if (nunits < NALLOC)
        nunits = NALLOC;

    cp = sbrk(nunits * sizeof(Header));

    if (cp == (char *) -1)
        return NULL;

    p = (Header *) cp;
    p->s.size = nunits;
    myfree((void *)(p+1));

    return freep;
}


void *mymalloc(int nbytes)
{
    Header *p, *prev;

    unsigned nunits = (nbytes + sizeof(Header) - 1) / sizeof(Header) + 1;

    if ((prev = freep) == NULL) {
        base.s.ptr = freep = prev = &base;
        base.s.size = 0;
    }

    for (p = prev->s.ptr, p = p->s.ptr;; prev = p, p = p->s.ptr) {
        if (p->s.size >= nunits) {

            if (p->s.size > nunits) {
                p->s.size -= nunits;
                p += p->s.size;
                p->s.size = nunits;
            } else {
                prev->s.ptr = p->s.ptr;
            }

            freep = prev;
            return (void *)(p+1);
        }

        if (p == freep)
            if ((p = morecore(nunits)) == NULL)
                return NULL;
    }

    return NULL;
}


/* lazy implementation assuming the sunshine case, where n
   is multiple of the sizeof(Header) */
void bfree(void *ap, unsigned n)
{
    Header *p;
    char *cp;
    unsigned nunits;

    /* take care of the alignment */
    for (cp = (char *)ap; ((unsigned long) cp) % sizeof(Header) != 0; cp++, n--)
        ;

    nunits = n - 1 / sizeof(Header) + 1;

    if (n % sizeof(Header) != 0 || nunits <= 1) {
        fprintf(stderr, "bfree called with invalid parameters!\n");
        exit(1);
    }

    p = (Header *)cp;
    p->s.size = nunits;

    myfree((void *)(p+1));
}


int main(int argc, char *argv[])
{
    Header *p = NULL;

    while (1) {
        p = (Header *) mymalloc(sizeof(Header));
        if (p == NULL) {
            exit(1);
        }
        bfree(p, sizeof(Header));
    }
}
