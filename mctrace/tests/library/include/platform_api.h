
#ifndef __MCTRACE_PLATFORM_API_H__
#define __MCTRACE_PLATFORM_API_H__

// MCTrace Platform API
//
// This header file provides the function prototypes for all functions
// that must be implemented to produce a complete implementation of the
// Platform API.

// platform_send:
//
// Called by the instrumented binary to initiate the exfiltration
// of global telemetry data. This function's only responsibility
// is to "send" (i.e. write) the specified telemetry data to a
// platform-specific exfiltration channel such as a local bus, network
// connection, or file handle. The function is provided with the
// following parameters:
//
// - channel_id: An abstract identifier for the "channel" on which to
//   send the data. The channel ID is specified as part of a
//   `send(channel_id)` statement within a probe. The channel ID allows
//   the probe writer and the platform API implementation to coordinate
//   the data exfiltration process, but the channel ID is abstract and
//   has no meaning within MCTrace itself.
//
// - data: A data buffer containing all of the global telemetry data in
//   a binary format. The data in the buffer should typically be treated
//   as opaque since the contents will be decoded outside of the process
//   using other MCTrace tools. The platform API implementation should
//   not rely on the memory layout of the buffer.
//
// - sz: The size of the data buffer.
void platform_send(uint32_t channel_id, void* data, uint32_t sz);

// platform_alloc_memory:
//
// Allocate a buffer of the specified size and return its address.
//
// - sz: the size of the buffer to allocate.
//
// Returns the address of the allocated buffer.
void* platform_alloc_memory(size_t sz);

// platform_timestamp:
//
// Returns the current value of a nanosecond timestamp counter. In
// keeping with DTrace semantics, this counter need not be a real-time
// clock value; instead the counter can increment from an arbitrary
// point in the past and is expected to be used only for relative
// computations.
uint64_t platform_timestamp();

#endif
