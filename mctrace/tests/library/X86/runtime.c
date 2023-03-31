#define _POSIX1_SOURCE 2

#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

void send(uint32_t fd, void* str, uint32_t sz) {
    ssize_t ret = 0;
    __asm__ __volatile__(
        "movq %[fd], %%rdi;"
        "movq %[str], %%rsi;"
        "movq %[sz], %%rdx;"
        "movq $1, %%rax;"
        "syscall;"
        : "=g" (ret)
        : [fd] "g" ((uint64_t)fd), [str] "g" (str), [sz] "g" ((size_t)sz)
        : "rdi", "rsi", "rdx", "rax"
    );
}

void* alloc_memory(size_t sz, char* file) {
    void* res = 0;
    __asm__ __volatile__(
        //Create/Truncate file
        // "movq $2, %%rax;"
        // "movq %[file], %%rdi;"
        // "movq %[open_flags], %%rsi;"
        // "movq %[open_mode], %%rdx;"
        // "syscall;"
        // //Truncate to size (store the returned fd from the previous one though)
        // "push %%rax;"
        // "movq %%rax, %%rdi;"
        // "movq %[sz], %%rsi;"
        // "movq $77, %%rax;"
        // "syscall;"
        // "pop %%rax;"
        //Mmap using the file descriptor from above
        //"movq %%rax, %%r8;"
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
        : [file] "g" (file), [open_flags] "i" (O_RDWR | O_CREAT | O_TRUNC), 
          [open_mode] "i" (S_IRWXU), [mmap_prot_mode] "i" (PROT_WRITE | PROT_READ),
          [mmap_flags] "i" (MAP_SHARED | MAP_ANONYMOUS), [sz] "g" (sz)
        : "rdi", "rsi", "rdx", "rax", "r8", "r9", "r10"
    );
    
    return res;
}

uint64_t timestamp() {
    struct timespec ts ;
    //void* pts = 0;

    __asm__ __volatile__(
    "movq $0, %%rdi;"
    //"movq %[pts], %%rsi;"
    "lea %[ts], %%rsi;"
    "movq $228, %%rax;"
    "syscall;"
    :
    : [ts] "g" (ts)
    : "rdi", "rsi", "rax"
    );
    return ts.tv_sec * 1000000000 + ts.tv_nsec;
} 