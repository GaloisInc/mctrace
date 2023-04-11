int cnt ;
long ts;
long elapsed ;
long cumulative ;

::read:entry {
	cnt = cnt + 1;
    ts = timestamp;
    elapsed = 0l;
}

::read:return {
  elapsed = timestamp - ts ;
  cumulative = cumulative + elapsed ;
}
