int storevalue;

::OS_SocketRecvFrom:return {
    // arg0 on return probes capture the returned value from the
    // function being probed
    storevalue = arg0;
}

::CFE_EVS_SendEvent:return {
    // Send all telemetry data
    send(100);
}
