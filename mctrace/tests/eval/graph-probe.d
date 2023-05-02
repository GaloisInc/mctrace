long ts;
long prog_cntr;
	

:::entry
{
	ts = timestamp;
	prog_cntr = ucaller;
	send(1);
}

