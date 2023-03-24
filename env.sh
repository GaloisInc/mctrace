
if ! echo $PATH | grep musl-gcc >/dev/null
then
    PATH=$(pwd)/musl-gcc/output/bin:$PATH
fi
