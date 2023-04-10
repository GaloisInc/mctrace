int cnt ;
long ts;
long elapsed ;
long cumulative ;

::memcpy:entry {
	cnt = cnt + 1;
    ts = timestamp;
    elapsed = 0l;
}

::memcpy:return {
  elapsed = timestamp - ts ;
  cumulative = cumulative + elapsed ;
}
