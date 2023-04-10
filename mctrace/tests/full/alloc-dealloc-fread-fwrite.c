#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#define BUF_SIZE (4096 * 1024 * 100)

char* target_file = "/tmp/dumpster.bin";

int main() {
  FILE* fi = fopen("/dev/random", "r");
  FILE* fo = fopen(target_file, "w+");
  for(int i = 0; i < 4; i++) {
    void* buf = calloc(BUF_SIZE, 1);
    fread(buf, 1, BUF_SIZE, fi);
    fwrite(buf, 1, BUF_SIZE, fo);
    free(buf);
  }
  fclose(fi);
  fclose(fo);
  return 0;
}
