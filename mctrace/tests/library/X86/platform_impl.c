#define _POSIX1_SOURCE 2

#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include "include/platform_api.h"

void platform_send(uint32_t val, void* str, uint32_t sz) {
    ssize_t ret = 0;
    void *valptr = &val;
    size_t valsz = sizeof(val);
    int fd = 1;
    __asm__ __volatile__(
        "movq %[fd], %%rdi;"
        "movq %[valptr], %%rsi;"
        "movq %[valsz], %%rdx;"
        "movq $1, %%rax;"
        "syscall;"
        : "=g" (ret)
        : [fd] "g" ((uint64_t)fd), [valptr] "g" (valptr), [valsz] "g" ((size_t)valsz)
        : "rdi", "rsi", "rdx", "rax"
    );
}

void* platform_alloc_memory(size_t sz) {
    void* res = 0;
    __asm__ __volatile__(
        "movq $-1, %%r8;"
        "movq $9, %%rax;"
        "movq $0, %%rdi;"
        "movq %[sz], %%rsi;"
        "movq %[mmap_prot_mode], %%rdx;"
        "movq %[mmap_flags], %%r10;"
        "movq $0, %%r9;"
        "syscall;"
        "movq %%rax, %[res];"
        : [res] "=g" (res)
        : [mmap_prot_mode] "i" (PROT_WRITE | PROT_READ),
          [mmap_flags] "i" (MAP_SHARED | MAP_ANONYMOUS),
          [sz] "g" (sz)
        : "rdi", "rsi", "rdx", "rax", "r8", "r9", "r10"
    );

    return res;
}

uint64_t platform_timestamp() {
    struct timespec ts ;

    __asm__ __volatile__(
    "movq $0, %%rdi;"
    "lea %[ts], %%rsi;"
    "movq $228, %%rax;"
    "syscall;"
    :
    : [ts] "g" (ts)
    : "rdi", "rsi", "rax"
    );
    return ts.tv_sec * 1000000000 + ts.tv_nsec;
}
