@mkdir .\build
@cd .\build

@cl ..\src\*.cpp /std:c++20 /W0 /wd4530 /Fe"./compiler" /link /LIBPATH:"..\lib\" libtcc.lib | Findstr /v "note:" 
@del .\*.obj

@cd ..\
