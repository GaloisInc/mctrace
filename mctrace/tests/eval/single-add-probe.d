int x;
::read:entry {
  x = x + 1;
  send(0xbeef);
  send(copyint32(0xface2));
}
