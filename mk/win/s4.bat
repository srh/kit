cd wrc
..\s3 main && cl main.obj dummy.c /Fes4b
s4b main && cl main.obj dummy.c /Fes4
move /y s4.exe ..\s4.exe
cd ..
