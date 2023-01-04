#define _POSIX1_SOURCE 2

#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

void send(int fd, void* str, size_t sz) {
    ssize_t ret = 0;
    __asm__ __volatile__(
        "movq %[fd], %%rdi;"
        "movq %[str], %%rsi;"
        "movq %[sz], %%rdx;"
        "movq $1, %%rax;"
        "syscall;"
        : "=g" (ret)
        : [fd] "g" (fd), [str] "g" (str), [sz] "g" (sz)
        : "rdi", "rsi", "rdx", "rax"
    );
}

void* alloc_memory(size_t sz, char* file) {
    void* res = 0;
    __asm__ __volatile__(
        //Create/Truncate file
        "movq $2, %%rax;"
        "movq %[file], %%rdi;"
        "movq %[open_flags], %%rsi;"
        "movq %[open_mode], %%rdx;"
        "syscall;"
        //Truncate to size (store the returned fd from the previous one though)
        "push %%rax;"
        "movq %%rax, %%rdi;"
        "movq %[sz], %%rsi;"
        "movq $77, %%rax;"
        "syscall;"
        "pop %%rax;"
        //Mmap using the file descriptor from above
        "movq %%rax, %%r8;"
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
          [mmap_flags] "i" (MAP_SHARED), [sz] "g" (sz)
        : "rdi", "rsi", "rdx", "rax", "r8", "r9", "r10"
    );
    
    return res;
}
