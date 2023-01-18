all: read-syscall read-write-syscall slow-read-write

read-syscall: read-syscall.c
	musl-gcc -static read-syscall.c -o read-syscall
	
read-write-syscall: read-write-syscall.c
	musl-gcc -static read-write-syscall.c -o read-write-syscall

slow-read-write: slow-read-write.c
	musl-gcc -static slow-read-write.c -o slow-read-write

clean:
	find . -name '*.o' -delete
	rm read-syscall
	rm read-write-syscall
	rm slow-read-write

