set -xe

zig build -freference-trace

./zig-out/bin/Pine test_files/main.pine 

cd ./out

fasm main.fasm
fasm pine_runtime.fasm
fasm lib.fasm

ld main.o pine_runtime.o lib.o -dynamic-linker /lib64/ld-linux-x86-64.so.2 -lc -o PineProgram

cd ..