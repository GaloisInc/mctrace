
#ifndef __MCTRACE_PLATFORM_API_H__
#define __MCTRACE_PLATFORM_API_H__

// Mctrace Platform API

// Write the contents of the specified buffer to the specified file
// descriptor.
void platform_send(uint32_t fd, void* str, uint32_t sz);

// Allocate a buffer of the specified size and return its address.
void* platform_alloc_memory(size_t sz);

// Return the current value of a nanosecond timestamp counter. This
// counter increments from an arbitrary point in the past and should
// only be used for relative computations.
uint64_t platform_timestamp();

#endif
