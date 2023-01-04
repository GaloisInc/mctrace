all: read-syscall read-write-syscall

read-syscall: read-syscall.c
	musl-gcc -static read-syscall.c -o read-syscall
	
read-write-syscall: read-write-syscall.c
	musl-gcc -static read-write-syscall.c -o read-write-syscall

clean:
	find . -name '*.o' -delete
	rm read-syscall
	rm read-write-syscall
