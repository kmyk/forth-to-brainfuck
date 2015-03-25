#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
char *code;
char *pptr;
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
void push(char *back) {
    while (pptr < code+len) {
        switch (*(pptr++)) {
            case '>': ++ptr; break;
            case '<': --ptr; break;
            case '+': ++(*ptr); break;
            case '-': --(*ptr); break;
            case '.': putchar(*ptr);    break;
            case ',': *ptr = getchar(); break;
            case '[': if (*ptr) { push(pptr);  } else { skip(); } break;
            case ']': if (*ptr) { pptr = back; } else { return; } break;
        }
    }
    assert (back == NULL);
}
void read(char *path) {
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
    assert (code[i] == EOF);
    code[i] = -1; // EOF is -1
    len = i;
    pptr = code;
    fclose(fh);
}
int main(int argc, char *argv[]) {
    assert (argc == 2);
    read(argv[1]);
    push(NULL);
    return 0;
}
