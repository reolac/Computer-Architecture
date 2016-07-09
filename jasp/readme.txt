Welcome to the JASP Toolkit
===========================

All the tools within the JASP toolkit are copyright Mark Burrell, except 
for the C-- cross-compiler which is copyright David Harrison.

The authors of this toolkit, and the distributors, cannot accept 
responsibility for any disruption, damage and/or loss to your data or 
computer system that may occur while using this package.  This is 
free software and comes with no warranty.


Required Hardware
=================
JASP toolkit programs will run on any PC running Windows 95 or above. 
The Aspen program and the cross compiler are available for the following 
platforms :- 

    A PC running DOS 5 or above 
    A Linux machine based on Wintel architecture (ELF binary) 

How to Install
==============
Follow the instructions in Appendix A of Fundamentals Of Computer
Architecture.  Appendix A is included as a PDF file in this distribution.

File List
=========
Each file in the JASP toolkit is listed below, together with a brief
description.

|----------------------------------------------------------------------------|
|jasper.exe     | The JASPer program.                                        |
|----------------------------------------------------------------------------|
|aspen.exe      | The 16-bit Aspen program, this will run on systems using   |
|               | DOS 5 and upwards.  Being a DOS program it can only        |
|               | understand DOS 8.3 filenames.                              |
|----------------------------------------------------------------------------|
|aspen32.exe    | The 32-bit Aspen program, this will run on Windows 95 and  |
|               | upwards.                                                   |
|----------------------------------------------------------------------------|
|aspen          | The Aspen linux ELF binary.                                |
|----------------------------------------------------------------------------|
|instruct.mco   | The basic instruction set.                                 |
|----------------------------------------------------------------------------|
|advanced.mco   | The 32-bit advanced instruction set.                       |
|----------------------------------------------------------------------------|
|jcc.exe        | The JASP C-- cross-compiler, this will run on Windows 95   |
|               | and upwards.                                               |
|----------------------------------------------------------------------------|
|jcc            | The JASP C-- cross-compiler, this is the linux ELF binary. |
|----------------------------------------------------------------------------|
|aspen.hco      | The Aspen help file.                                       |
|----------------------------------------------------------------------------|
|jasper.hlp     | The JASPer help file.                                      |
|----------------------------------------------------------------------------|
|basicio.lib    | The basic I/O library.                                     |
|----------------------------------------------------------------------------|
|instruct.txt   | The basic instruction set quick reference.                 |
|----------------------------------------------------------------------------|
|advanced.txt   | The 32-bit advanced instruction set quick reference.       |
|----------------------------------------------------------------------------|
|jasm.pl        | The JASP cross-assembler, this will run on systems that    |
|               | have a PERL installation.                                  |
|----------------------------------------------------------------------------|
|jasm.bat       | A DOS-wrapper for jasm.pl - this can be used to run the    |
|               | assembler.                                                 |
|----------------------------------------------------------------------------|
|advancedio.lib | The advanced I/O library, to be used with 32-bit programs. |
|----------------------------------------------------------------------------|
|hello.jas      | An example 'Hello World' program.                          |
|----------------------------------------------------------------------------|
|clock.jas      | An example program that displays the current system time.  |
|----------------------------------------------------------------------------|
|appendix.pdf   | Appendix A of Fundamentals Of Computer Architecture.  This |
|               | file contains information about the JASP toolkit, how to   |
|               | set it up, and how to use it.                              |
|----------------------------------------------------------------------------|
