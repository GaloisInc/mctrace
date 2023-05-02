int cnt ;
long ts;
long elapsed ;
long cumulative ;

::fread:entry {
	cnt = cnt + 1;
    ts = timestamp;
    elapsed = 0l;
}

::fread:return {
  elapsed = timestamp - ts ;
  cumulative = cumulative + elapsed ;
}
