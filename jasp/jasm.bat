@echo off
rem
rem   This DOS wrapper for the JASPer assembler can either
rem   run the assembler from the current location, or
rem   more suitably can run it from a the directory listed
rem   in the JASP environment variable.  You should best
rem   set this variable in your autoexec.bat file to be used
rem   by all JASP software, or you can set it here.
rem
rem   To set the variable you can use something like: 
rem   SET JASP=c:\jasp\
rem
perl "%JASP%jasm.pl" %1 %2 %3 %4 %5 %6 %7 %8 %9
