int x;
::read:entry {
  x = x + 1;
  send(0xbeef);
}
