cd vrc
..\s2 main && cl main.obj dummy.c /Fes3b /link /stack:0x100000,0x100000
s3b main && cl main.obj dummy.c /Fes3
del ..\s3.exe
move s3.exe ..\s3.exe
cd ..
