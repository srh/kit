cd xrc
..\s4 main && cl main.obj dummy.c /Fes5b
s5b main && cl main.obj dummy.c /Fes5
move /y s5.exe ..\s5.exe
cd ..
