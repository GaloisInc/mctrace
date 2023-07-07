int read_count;
::read:entry {
  read_count = read_count + 1;
  send(0xff);
}


int write_count;
::write:entry {
  write_count = write_count + 1;
  send(0xff);
}
