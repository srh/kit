cd vrc
..\s2 main && cl main.obj dummy.c /Fes3b
s3b main && cl main.obj dummy.c /Fes3
move /y s3.exe ..\s3.exe
cd ..
