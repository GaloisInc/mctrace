int write_count;
int read_count;
long write_ts;
long write_elapsed;
long read_ts;
long read_elapsed;

::write:entry {
      write_count = write_count + 1;
      write_ts = timestamp;
      write_elapsed = 0l;
}

::write:return {
      write_elapsed = timestamp - write_ts;
      write_ts = timestamp ;
      send(2);
}

::read:entry {
      read_count = read_count + 1;
      read_ts = timestamp;
      read_elapsed = 0l;
}

::read:return {
      read_elapsed = timestamp - read_ts;
      read_ts = timestamp ;
      send(2);
}
