int write_count;
int write_return;
long write_ts;
long write_elapsed;
long prog_cntr;

::write:entry {
  write_count = write_count + 1;
  write_return = 0;
  write_ts = timestamp;
  write_elapsed = 0l;
  prog_cntr = ucaller;
}

::write:return {
  write_return = 1;
  write_elapsed = timestamp - write_ts;
  prog_cntr = ucaller;
}
