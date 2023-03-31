all: x86 ppc32

x86: x86-read-syscall x86-read-write-syscall x86-slow-read-write x86-alloc-dealloc-fread-fwrite

x86-read-syscall: read-syscall.c
	musl-gcc -static read-syscall.c -o x86-read-syscall
	
x86-read-write-syscall: read-write-syscall.c
	musl-gcc -static read-write-syscall.c -o x86-read-write-syscall

x86-slow-read-write: slow-read-write.c
	musl-gcc -static slow-read-write.c -o x86-slow-read-write
	
x86-alloc-dealloc-fread-fwrite: alloc-dealloc-fread-fwrite.c
	musl-gcc -static alloc-dealloc-fread-fwrite.c -o x86-alloc-dealloc-fread-fwrite
	
ppc32: ppc32-read-syscall ppc32-read-write-syscall ppc32-slow-read-write ppc32-alloc-dealloc-fread-fwrite

ppc32-read-syscall: read-syscall.c
	powerpc-linux-muslsf-gcc -static read-syscall.c -o ppc32-read-syscall
	
ppc32-read-write-syscall: read-write-syscall.c
	powerpc-linux-muslsf-gcc -static read-write-syscall.c -o ppc32-read-write-syscall

ppc32-slow-read-write: slow-read-write.c
	powerpc-linux-muslsf-gcc -static slow-read-write.c -o ppc32-slow-read-write
	
ppc32-alloc-dealloc-fread-fwrite: alloc-dealloc-fread-fwrite.c
	powerpc-linux-muslsf-gcc -static alloc-dealloc-fread-fwrite.c -o ppc32-alloc-dealloc-fread-fwrite

clean:
	find . -name '*.o' -delete
	rm x86-*
	rm ppc32-*

