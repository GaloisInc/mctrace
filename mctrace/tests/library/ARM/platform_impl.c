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

void platform_send(uint32_t fd, void* str, uint32_t sz) {
}

void* platform_alloc_memory(size_t sz) {
    return NULL;
}

uint64_t platform_timestamp() {
    return 0;
}
