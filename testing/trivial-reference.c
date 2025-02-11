/**
 * Build:
 *
 * gcc -I./testing -I. -I./include -Iexternal/crc32c/include \
 *      testing/trivial-reference.c external/crc32c/src/crc32c.cc \
 *      external/crc32c/src/crc32c_portable.cc -lz
 */
#include <stdio.h>
#include <zlib.h>
#include "crc32c/crc32c.h"

typedef uLong HashFunc(uLong seed, const Bytef *buf, uInt len);

static void printHash(uLong hash)
{
    printf("    %lu\n", (unsigned long) hash);
}

static void runTest(const char *label, HashFunc func, uLong seed)
{
    printf("%s\n", label);

    printHash(func(seed, (const Bytef *)"",      0));
    printHash(func(seed, (const Bytef *)"",      0));
    printHash(func(seed, (const Bytef *)"\0",    1));
    printHash(func(seed, (const Bytef *)"a",     1));
    printHash(func(seed, (const Bytef *)"hello", 5));

    {
        int i;
        unsigned char buffer[300];

        for (i = 0; i <= 255; i++)
            buffer[i] = i;

        printHash(func(seed, buffer, 256));
    }

    puts("");
}

int main(void)
{
    runTest("adler32",                  adler32, 1);
    runTest("adler32Update 0",          adler32, 0);
    runTest("adler32Update 1",          adler32, 1);
    runTest("adler32Update 123",        adler32, 123);
    runTest("adler32Update 0xFFF0FFF0", adler32, 0xFFF0FFF0);

    runTest("crc32",                    crc32, 0);
    runTest("crc32Update 0",            crc32, 0);
    runTest("crc32Update 1",            crc32, 1);
    runTest("crc32Update 123",          crc32, 123);
    runTest("crc32Update 0xFFFFFFFF",   crc32, 0xFFFFFFFF);

    runTest("crc32c",                   crc32c_extend, 0);
    runTest("crc32cUpdate 0",           crc32c_extend, 0);
    runTest("crc32cUpdate 1",           crc32c_extend, 1);
    runTest("crc32cUpdate 123",         crc32c_extend, 123);
    runTest("crc32cUpdate 0xFFFFFFFF",  crc32c_extend, 0xFFFFFFFF);

    return 0;
}
