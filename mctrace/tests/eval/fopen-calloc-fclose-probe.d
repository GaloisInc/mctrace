long elapsed;
long ts;
long prog_cntr;

::fopen:entry, ::fclose:entry, ::calloc:entry
{
    ts = timestamp;
    prog_cntr = ucaller;
}

::fopen:return, ::fclose:return, ::calloc:return
{
    elapsed = timestamp - ts;
    prog_cntr = ucaller;
    send(0xff);
}
