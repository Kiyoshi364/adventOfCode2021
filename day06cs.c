#include <stdio.h>
#include <stdlib.h>

#define TAM 9

void p(long long pool[TAM]) {
    for ( int i = 0; i < TAM; i++ ) {
        printf("%lld ", pool[i]);
    }
    printf("\n");
}

int main () {
    long long pool[TAM];
    for ( int i = 0; i < TAM; i++ ) {
        pool[i] = 0;
    }

    // parsing
    int c;
    while ((c = getchar()) != EOF) {
        if ( '0' <= c && c <= '8' ) {
            int i = c - '0';
            pool[i] += 1;
        } else if ( c == ',' || c == '\r' || c == '\n' ) {
            // Ignoring !
        } else {
            printf("Unexpected char `%c` (%d)\n", (char) c, c);
        }
    }
    // p(pool);

    // breeding
    for ( int d = 0; d < 256; d++ ) {
        long long p0 = pool[0];
        for ( int i = 0; i < TAM-1; i++ ) {
            pool[i] = pool[i+1];
        }
        pool[6] += p0;
        pool[8] = p0;
    }

    // fold (+) 0
    long long acc = 0;
    for ( int i = 0; i < TAM; i++ ) {
        acc += pool[i];
    }

    printf("acc: %lld\n", acc);
}
