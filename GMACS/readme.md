# GMACS read me 

## Versions

The latest GMACS version and history of updates to the .tpl are found here:

[GMACS GitHub](https://github.com/GMACS-project/GMACS_tpl-cpp_code)  

The latest version used by ADF&G assessments is GMACS 2.01.M.10; Completed 2024-02-27.

## Compiling

To compile GMACS and create an executible file, download the contents of the src_* subdirectory. Then open ADMB shell and navigate the working directory to the location of the files. Type the command, "make.bat". This should run the make file and create the gmacs.exe.

## Running GMACS

Once the gmacs.exe is created it has to be in a subdirectory with four files: 1) gmacs.dat - a pointer and general setup file, 2) .dat file - contains model data, 3) .ctl file - contains parameterization controls, and 4) .prj file - contains projection controls. Run GMACS by opening the exe file or using the GMACS interface with R (documentation in development). To troubleshoot model errors, use checkfile.rep or the various model generated gmacs_in files, which mirror expected input files.
