@mkdir .\build
@cd .\build

@cl ..\src\*.cpp /std:c++20 /W0 /wd4530 /D_AMD64_ /DWIN64 /Fe"./compiler" /link /LIBPATH:"..\lib\" libtcc.lib | Findstr /v "note:" 
@del .\*.obj

@cd ..\
