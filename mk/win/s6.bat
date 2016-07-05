cd src
..\s5 main && cl main.obj dummy.c /Fes6b
s6b main && cl main.obj dummy.c /Fes6
move /y s6.exe ..\s6.exe
cd ..
