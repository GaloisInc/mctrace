int arg;
int ch;
::array_sum:entry {
  arg = arg0;
  ch = copyint32(arg);
  send(1);
}

::array_sum:return {
    arg = arg0;
    ch = 0;
    send(1);
}
