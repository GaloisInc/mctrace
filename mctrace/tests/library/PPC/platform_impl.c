#define _POSIX1_SOURCE 2

#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <inttypes.h>
#include <time.h>

#include "include/platform_api.h"

static void write_stderr(void* str, uint32_t sz) ;

void platform_send(uint32_t fd, void* str, uint32_t sz) {
    //Currently we *always* force the write to go to stderr
    //(file descriptor = 2) irrespective of what was passed
    //in as the value of fd
    write_stderr(str, sz);
}

void* platform_alloc_memory(size_t sz) {
    void* res = 0;
    __asm__ __volatile__(
        "li %%r0, 90;"
        "li %%r3, 0;"
        "ori %%r4, %[sz], 0;"
        "li %%r5, %[mmap_prot_mode];"
        "li %%r6, %[mmap_flags];" 
        "li %%r7, -1;"
        "li %%r8, 0;"
        "sc;"
        "ori %[res], %%r3, 0;"
        : [res] "=r" (res)
        : [open_flags] "i" (O_RDWR | O_CREAT | O_TRUNC), 
          [open_mode] "i" (S_IRWXU), [mmap_prot_mode] "i" (PROT_WRITE | PROT_READ),
          [mmap_flags] "i" ((char)(MAP_SHARED | MAP_ANONYMOUS)),
          [sz] "r" (sz)
        : "r0", "r3", "r4", "r5", "r6", "r7", "r8"
    );

    return res;
}

uint64_t platform_timestamp() {
    struct timespec ts = { 0 } ;

    __asm__ __volatile__(
    "li %%r3, 0;"
    "ori %%r4, %[ts], 0;"
    "li %%r0, 403;"
    "sc;"
    :
    : [ts] "r" (&ts)
    : "r3", "r4", "r0"
    );

    return ts.tv_sec * 1000000000 + ts.tv_nsec;
}

static void write_stderr(void* str, uint32_t sz) {
    ssize_t ret = 0;
    __asm__ __volatile__(
        "ori %%r3, %[fd], 0;"
        "ori %%r4, %[str], 0;"
        "ori %%r5, %[sz], 0;"
        "li %%r0, 4;"
        "sc;"
        : "=g" (ret)
        : [fd] "r" (2), [str] "r" (str), [sz] "r" ((size_t)sz)
        : "r3", "r4", "r5", "r0"
    );
}
