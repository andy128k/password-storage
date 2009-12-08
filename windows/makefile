all:
	windres runner.rc -O coff -o runner.res
	gcc -o runner.exe runner.c runner.res -mwindows -lkernel32
