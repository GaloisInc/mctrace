#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PROBE_BUF_SIZE 128

// The type of probes
//
// They take a single argument that points to their global storage
typedef void(*probe_type)(void*);

// Return true if there is a NUL character in the buffer
//
// If there is a newline, replace it with a NUL (fgets includes the newline, but
// we don't want to pass it to dlsym)
static int validateProbeName(char buffer[PROBE_BUF_SIZE]) {
  for(int i = 0; i < PROBE_BUF_SIZE; ++i) {
    if(buffer[i] == '\n') {
      buffer[i] = 0;
      return 1;
    } else if(buffer[i] == 0) {
      return 1;
    }
  }

  return 0;
}

// Return the handle to the named function
//
// This will return NULL if the function was not found
static probe_type loadProbe(void* hdl, const char* probeName) {
  return (probe_type)dlsym(hdl, probeName);
}

// We expect two arguments:
//
// 1. The path to the shared library to dlopen
//
// 2. The number of bytes to allocate to store "global" probe data
int main(int argc, char* argv[]) {
  if(argc != 3) {
    fprintf(stderr, "Usage: probe-runner /path/to/probes.so NUM_BYTES\n");
    return 1;
  }

  const char* probesPath = argv[1];
  errno = 0;
  long nBytes = strtol(argv[2], NULL, 10);
  if(errno != 0) {
    fprintf(stderr, "Error parsing number of bytes to allocate for global variables: %s\n", argv[2]);
    return 2;
  }

  // Allocate the requested amount of global storage for the probes
  void* globalStorage = calloc(nBytes, 1);
  void** globalVar = &globalStorage;
  void* hdl = dlopen(probesPath, RTLD_NOW);
  if(!hdl) {
    fprintf(stderr, "Error loading shared library %s\n", probesPath);
    return 3;
  }

  char buffer[PROBE_BUF_SIZE];
  memset(buffer, 0, PROBE_BUF_SIZE);

  // Read probe names from stdin and run the named probe (by grabbing a handle
  // to it with dlsym).  Continue until EOF (of stdin)
  while(fgets(buffer, PROBE_BUF_SIZE, stdin)) {
    if(!validateProbeName(buffer)) {
      fprintf(stderr, "Invalid probe name: %s\n", buffer);
      return 4;
    }

    probe_type probe = loadProbe(hdl, buffer);
    if(!probe) {
      fprintf(stderr, "Could not load probe: %s\n", buffer);
      return 5;
    }
    probe(globalVar);
    memset(buffer, 0, PROBE_BUF_SIZE);
  }

  // Save the resulting global storage to stdout
  fwrite(globalStorage, 1, nBytes, stdout);
  fclose(stdout);
  free(globalStorage);

  return 0;
}
