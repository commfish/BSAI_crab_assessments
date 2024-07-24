if NOT EXIST build mkdir build
copy lib\*.cpp
copy gmacsbase.tpl+personal.tpl gmacs.tpl
call admb -g gmacs.tpl tailcompression.cpp nloglike.cpp spr.cpp multinomial.cpp robust_multi.cpp equilibrium.cpp dirichlet.cpp 
:: g++ -c -std=c++14 -O3 -fpermissive -D_FILE_OFFSET_BITS=64 -I. -I"C:\Program Files (x86)\ADMB\include" -I"C:\Program Files (x86)\ADMB\contrib\include" -o gmacs.obj gmacs.cpp
:: g++ -static -g -o gmacs.exe gmacs.obj "C:\Program Files (x86)\ADMB\lib\libadmb-contrib.a" tailcompression.obj nloglike.obj spr.obj multinomial.obj robust_multi.obj equilibrium.obj dirichlet.obj
copy gmacs.exe build\AIGKC
copy gmacs.exe build\BBRKC
copy gmacs.exe build\SMBKC
copy gmacs.exe build\NSRKC
copy gmacs.exe build\snow_crab

rem copy gmacs.exe build\SMBKC
ren copy gmacs.exe build\EAG21_8e
ren copy gmacs.exe build\WRL
rem copy gmacs.exe build\BBRKC
rem copy gmacs.exe build\Runs
rem copy gmacs.exe build\NSRKC
rem copy gmacs.exe build\WAGG
rem copy gmacs.exe build\jie
rem copy gmacs.exe build\debug2
rem copy gmacs.exe build\debug3
rem copy gmacs.exe build\BBRKC
rem copy gmacs.exe build\"St Matt"
rem copy gmacs.exe build\Snow
rem call admb -f gmacs.tpl tailcompression.cpp nloglike.cpp spr.cpp multinomial.cpp robust_multi.cpp equilibrium.cpp dirichlet.cpp
rem copy gmacs.exe build\release
:: Cleanup src directory (these files live in lib directory)
del tailcompression.cpp nloglike.cpp spr.cpp multinomial.cpp robust_multi.cpp equilibrium.cpp dirichlet.cpp
rem del gmacs.exe

