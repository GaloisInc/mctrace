int can_read_count;
long can_read_ts;
long can_read_elapsed;

int can_write_count;
long can_write_ts;
long can_write_elapsed;

::can_read:entry {
      can_read_count = can_read_count + 1;
      can_read_ts = timestamp;
      can_read_elapsed = 0l;
}

::can_read:return {
      can_read_elapsed = timestamp - can_read_ts;
      can_read_ts = timestamp ;
      send(2);
}

::can_write:entry {
      can_write_count = can_write_count + 1;
      can_write_ts = timestamp;
      can_write_elapsed = 0l;
}

::can_write:return {
      can_write_elapsed = timestamp - can_write_ts;
      can_write_ts = timestamp ;
      send(2);
}
