
#ifndef __MCTRACE_PLATFORM_API_H__
#define __MCTRACE_PLATFORM_API_H__

// Mctrace Platform API

// This function is called by the instrumented binary to initiate
// the exfiltration of global telemetry data. The function is
// provided with the following parameters:
// 1. channel_id: An identifier for the "channel" on which to send the data
//      The channel id is specified as part of a `send(channel_id)` statement
//      within a probe. The channel id allows the probe writer and the platform
//      api to coordinate and (re)direct the data exfiltration process, but has
//      no ascribed meaning within MCTrace
// 2. data: A data buffer containing all the telemetry data in a binary format.
//      The data in the buffer should typically be treated as a black box and 
//      the contents interpreted outside of the process using other MCTrace
//      facilities.
// 3. sz: The size of the data buffer
void platform_send(uint32_t channel_id, void* data, uint32_t sz);

// This function, when called, should allocate a buffer of the specified size and
// return its address.
void* platform_alloc_memory(size_t sz);

// Return the current value of a nanosecond timestamp counter. In keeping with
// DTrace semantics, this counter need not be a real-time clock value; instead
// the counter can increment from an arbitrary point in the past and is expected
// to be used only for relative computations.
uint64_t platform_timestamp();

#endif
