cd urc
..\out main && cl main.obj dummy.c /Fes2b /link /stack:0x100000,0x100000
s2b main && cl main.obj dummy.c /Fes2 /link /stack:0x100000,0x100000
del ..\s2.exe
move s2.exe ..\s2.exe
cd ..
