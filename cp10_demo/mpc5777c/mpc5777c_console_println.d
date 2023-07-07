int write_count;
long write_ts;
long write_elapsed;

::console_println:entry {
      write_count = write_count + 1;
      write_ts = timestamp;
      write_elapsed = 0l;
}

::console_println:return {
      write_elapsed = timestamp - write_ts;
      write_ts = timestamp ;
      send(2);
}
