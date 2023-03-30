int write_count;
long write_ts;
long write_elapsed;


::write:entry {
  write_count = write_count + 1;
  write_ts = timestamp;
  write_elapsed = 0l;
}

::write:return {
  write_elapsed = timestamp - write_ts;
  write_ts = timestamp ;
}
