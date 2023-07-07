int read_argval;
int read_retval;
int write_argval;
int write_retval;

::read:entry {
  read_argval = arg0 ;
  send(100);
}

::read:return {
  read_retval = arg0 ;
  send(100);
}

::write:entry {
  write_argval = arg0 ;
  send(100);
}

::write:return {
  write_retval = arg0 ;
  send(100);
}