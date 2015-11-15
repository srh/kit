cd urc
..\out main && cl main.obj dummy.c /Fes2b
s2b main && cl main.obj dummy.c /Fes2
move /y s2.exe ..\s2.exe
cd ..
