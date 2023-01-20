#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

#define BUF_SIZE (4096 * 1024 * 100)

char* target_file = "/tmp/dumpster.bin";

int main() {
  unlink(target_file)  ;
  
  char* buffer = malloc(BUF_SIZE);  

  for(int i = 0; i < 5; ++i) {
    int fd = open("/dev/random", O_RDONLY);
    read(fd, buffer, BUF_SIZE);
    close(fd);
    
    sleep(1);
    
    fd = open(target_file, O_WRONLY | O_APPEND | O_CREAT, S_IRWXU);    
    write(fd, buffer, BUF_SIZE);
    close(fd);
  }
  
  return 0;
}
