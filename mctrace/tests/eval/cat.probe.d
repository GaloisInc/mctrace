int cnt ;
long ts;
long elapsed ;
long cumulative ;

::safe_read:entry {
	cnt = cnt + 1;
    ts = timestamp;
    elapsed = 0l;
}

::safe_read:return {
  elapsed = timestamp - ts ;
  cumulative = cumulative + elapsed ;
}
