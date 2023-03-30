#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>


#define BUF_SIZE 100
char buffer[BUF_SIZE];

int g = 5;

int main() {
  int fd = open("/dev/zero", 0);
  for(int i = 0; i < 4; i++) {
      read(fd, buffer, g);
  }
  close(fd);

  fd = open("/dev/null", 1);
  for(int i = 0; i < 7; i++) {
      write(fd, buffer, g);
  }
  close(fd);
  
  return 0;
}
