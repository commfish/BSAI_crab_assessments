# GMACS read me 

# update 3-9-2023
In folder you need: 
'lib' folder
'include' folder
make.bat
personal.tpl
gmacsbase.tpl

navigate using ADMB shell CMD to the folder that the above are stored. Once there type "make.bat".
This should run the make file and create the gmacs.exe 

Once the gmacs.exe is created it has to be in the folder with the input files for each model run. See model run specfic readme files for those instructions.

# compile GMACS 

- open command line shell to folder with GMACS files - library files plus, gmacsbase.tpl and personal.tpl
- type into command line 'make.bat'
- gmacs should compile and produce 'gmacs.exe' file - this is the file you need to have in the folder with your model runs.


## Annotated with updates that I've made of others have made

04-22-2020
Updated .tpl to include fishing mortality in the .rep file
Update .tpl to output Dynamic B0 - per Jim I's instructions from last year

01-30-2020
Many updates from Andre - see his notes and e-mails.