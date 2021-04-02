#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>


#define BUF_SIZE 100
char buffer[BUF_SIZE];

int g = 5;

int main() {
  int fd = open("/dev/zero", 0);
  read(fd, buffer, g);
  return 0;
}
