int entered;
int exited;

::read:entry {
  entered = entered + 1;
}

::read:return {
  exited = exited + 1;
}
