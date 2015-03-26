#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
char *code;
const char *pptr;
int len;
uint8_t mem[30000] = {};
uint8_t *ptr = mem;
void skip(void) {
    while (pptr < code+len) {
        switch (*(pptr++)) {
            case '[': skip(); break;
            case ']': return;
        }
    }
    assert (NULL);
}
void push(const char *back) {
    while (pptr < code+len) {
        switch (*(pptr++)) {
            case '>': ++ptr; break;
            case '<': --ptr; break;
            case '+': ++(*ptr); break;
            case '-': --(*ptr); break;
            case '.': putchar(*ptr);    break;
            case ',': *ptr = getchar(); if (*ptr == EOF) { *ptr = -1; } break; // EOF is -1
            case '[': if (*ptr) { push(pptr);  } else { skip(); } break;
            case ']': if (*ptr) { pptr = back; } else { return; } break;
        }
    }
    assert (back == NULL);
}
char *read(char *path) {
    int n = 512;
    int i = 0;
    FILE *fh = fopen(path, "r");
    code = (char*)malloc(n);
    while ((code[i] = fgetc(fh)) != EOF) {
        i += 1;
        if (i == n) {
            n *= 2;
            code = (char*)realloc(code, n);
        }
    }
    fclose(fh);
}
int main(int argc, char *argv[]) {
    assert (argc == 2);
    code = read(argv[1]);
    pptr = code;
    len = strlen(code);
    push(NULL);
    free(code);
    return 0;
}
