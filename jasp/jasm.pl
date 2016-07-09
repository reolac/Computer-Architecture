#!/usr/bin/perl
#
# The JASP assembler
#
# Uses the details in an instruction set to assemble an assembly file
# into machine code that can then be loaded into JASPer or Aspen.
#
# Run the program something like :-
#
#    jasm.pl -a assemblerfile 
#           [-m instructionfile]
#           [-l default|debug|code|printout]
#           [-o outputfile]
# 
# For further details type :-
#
#    jasm.pl -h
#
# This is free software and comes with no warranty.
#-----------------------------------------------------------------------------
use strict;


# Configuration changes allowed in this section
#=============================================================================

# The default microcode file
my $defaultMCO = "instruct.mco";

# Configuration forbidden below this line
#=============================================================================

use Getopt::Std;    # import options library

my ($copyrightString) = "JASP Assembler (c) 1999-2003 Mark S. Burrell";
my ($emailString)     = "mark\@brittunculi.com";
my ($versionString)   = "V1.1";
my ($versionDate)     = "03-Jul-2003";

our($opt_o, $opt_m, $opt_a, $opt_l, $opt_h);

my $mco; # microcodes
my $asp; # input assembly program

# type of output to produce
my $displayOutput; # could be 'default'|'debug'|'code'|'printout'
my $outputFile;    # output filename if used

getopts('hm:a:l:o:');

if ($opt_m) { $mco           = $opt_m ; } # mco file
if ($opt_a) { $asp           = $opt_a ; } # asp file
if ($opt_l) { $displayOutput = $opt_l ; } # run type

# we are asking to output to file
if ($opt_o) { 
    $outputFile = $opt_o;
    open(OUTFILE, ">$outputFile") || fatalError("Couldn't open output file [$outputFile]");    
} 

# if help asked for, or no assembler file given
if (($opt_h)||(!$opt_a)) {
    printHelp();
}

# Force 32 bit instructions if required.
# Set by a flag in the MCO file
my ($FORCE_32BIT) = 0;

#
# list of accepted directives
#
my (@DIRECTIVES) = ('ORG', 'DC.B', 'DC.W', 'DS.W', 'EQU');

#
# list of accepted registers
#
my (@REGISTERS) = qw(A B PC INC SP ALUX ALUY ALU MAR MDR PSR CU IR);

#
# list of synonyms for 8/16-bit data/address/displacement
#
my (@DATA8 ) = ('DATA', 'DATABYTE', 'DATAB', 'DB', 'DATA8'); #, 'DATA.B');
my (@DATA16) = ('DATAWORD', 'DATAW', 'DW', 'DATA16'); #, 'DATA.W');

my (@ADDR8 ) = ('ADDR', 'ADDRBYTE', 'ADDRB', 'AB', 'ADDR8'); #,'ADR', 'ADDR.B');
my (@ADDR16) = ('ADDRWORD', 'ADDRW', 'AW', 'ADDR16'); #, 'ADDR.W');

my (@DIS8  ) = ('DIS', 'DISBYTE', 'DISB', 'DIS8'); #, 'DIS.B');
my (@DIS16 ) = ('DISWORD', 'DISW', 'DIS16'); #, 'DIS.W');

#
# storage for various lists
#
my (@ALLCMDS) = @DIRECTIVES;

my (%INSTRUCTIONS) = (); # instructions
my (%MNEMONICS)    = (); # instructions keyed on mnemonic
my (%SYMBOLS)      = (); # symbols
my (@SOURCE)       = (); # ASM source code
my (@ASM)          = (); # assembled code
my (@ERRORS)       = (); # stores all the assembly errors

#
# some variables
#
my($ERRORS)        = 0;  # number of assembly errors
my ($myerror)      = ""; # used to build error message

my $ADDRESS = 0; # global variable, storage for current address

my ($retval, $instruction, $params, @params, $i, $symbol, $data, $mynum);
my ($count, $result, $size, $char, @chars, $c, $junk, $junk2);

my ($myindex, $mynewstring, @mychars, $quotes);

# use the default display type if no other listed on the command line
$displayOutput = "default" if (! $displayOutput);

# use the default instruction set if no other listed on the command line
$mco = $defaultMCO if (! $mco);

readMCO($mco);  # read the MCO file - establish %INSTRUCTIONS
readASM($asp);  # read the ASM file - establish @SOURCE

loadSymbols();  # load symbols into %SYMBOLS

# analyse the assembly source
for($i = 1; $i <= $#SOURCE; $i++) {
    # look at each source line in turn
    ($retval, $instruction, $params, $symbol) = &analyseLine($i);
    if ($retval) {

        #
        # separate out params into an array
        #
        @params = split(/\,/, $params);

        #
        # check for each directive
        #
        if ($instruction =~ /^DC\.B$/i) {
            saveLine("", $i, $ADDRESS);
            if ($SOURCE[$i] =~ /DC\.B(\s+)(.*)$/i ) {
                $char = $2;
                # we need to cope with parameters like 
                # 00,'fred\n\n',0d
                #
                
                @mychars = split(//,$char);
                $mynewstring = "";
                $quotes = 0;
                for ($myindex = 0; $myindex <= $#mychars; $myindex++) {
                    if (($mychars[$myindex] eq "'") && ($mychars[$myindex -1] ne "\\")) {
                        if ($quotes == 0) { $quotes = 1; } else { $quotes = 0; }
                    }
                    
                    if (($mychars[$myindex] eq ",") && ($quotes == 1) ){
                        $mynewstring = $mynewstring . "\\,";
                    }
                    else {
                        $mynewstring = $mynewstring . $mychars[$myindex];
                    }
                }
                if ($quotes == 1) {
                    reportError($i, 1, "Unfinished text string used");
                }
                else {
                
                    $char = normaliseChars($mynewstring);

                    @chars = split(/\,/, $char);
                    $mynum = "";
                    foreach $char (@chars) {
                        $char =~ s/^\s+//;
                        $char =~ s/\s+$//;
                        $c = hexNumber($char, $i);
                        $c = hex($c);
                        $c = formatNumber($c, 2);
                        $mynum  = $mynum . "$c";
                    }
                    if ( ( dataByteSize($mynum) % 2) == 1) {
                        $mynum = $mynum . "00";
                    }
        
                    saveLine("$mynum", $i, $ADDRESS);
            
                    $mynum = dataByteSize($mynum) /2;
                    $ADDRESS = $ADDRESS + $mynum;                
                }
            }
            else {
                $myerror = "I can't understand this DC.B instruction\n" .
                           "Are you using a valid format?";
                reportError($i, 1, $myerror);
                           
            }
            
        }
        elsif ($instruction =~ /^DC\.W$/i) {
            saveLine("", $i, $ADDRESS);
            if ($SOURCE[$i] =~ /DC\.W(\s+)(.*)$/i ) {
                $char = $params;

                $mynum = "";
                $char =~ s/^\s+//;
                $char =~ s/\s+$//;
                
                # test to see if the value has a leading #, if it
                # has then it is a label, otherwise it is a number
                if (isNumber($char)) {
                    # number
                    $c = hexNumber($char, $i);
                    $c = hex($c);
                    
                    if (is16bit($c)) {
                        $c = formatNumber($c, 4);
                        $mynum  = $mynum . "$c";
                    }
                    else {
                        # number too big
                        $myerror = "DC.W parameter must be 16-bits.\n" .
                                   "The number you have used is too large";
                        reportError($i, 1, $myerror);
                    }
                } 
                else {
                    # is label
                    $mynum = $char;
                    ($junk, $mynum) = split(/#/, $mynum, 2);
                    if (isSymbol($mynum) ) {
                        # sendOutput "return [$num]\n";
                        $mynum = ":$mynum!16A";
                    }
                    else {
                        $myerror = "DC.W parameter is not a valid symbol.\n" .
                                   "It must either be a number, or a valid label";
                        reportError($i, 1, $myerror);
                        $mynum = "";
                    }
                }
                saveLine("$mynum", $i, $ADDRESS);
        
                $mynum = dataByteSize($mynum);

                $mynum = $mynum /2;
                $ADDRESS = $ADDRESS + $mynum;
            }
            else {
                # this needs to be downgraded to an error
                $myerror = "I can't understand this DC.W instruction\n" .
                           "Are you using a valid format?";
                reportError($i, 1, $myerror);
            }
        }
        elsif ($instruction =~ /^DS\.W$/i) {
            saveLine("", $i, $ADDRESS);
            if ($#params != 0) {
                reportError($i, 1, "1 DS.W parameter expected, which must be a number");
            }
            else {
                if (isNumber($params[0])) {
                    $mynum = hexNumber($params[0]);
                    $mynum = hex($mynum);
                    if ($mynum > 0) {
                        $result = "";
                        for ($count = 0; $count < $mynum; $count++) {
                            $result .= "0000";
                        }
                        saveLine("$result", $i, $ADDRESS);
            
                        $size = dataByteSize($result);
                        $size = $size / 2;
                        $ADDRESS = $ADDRESS + $size;
                    }
                    else {
                        reportError($i, 1, "DS.W parameter must be greater than zero");                        
                    }
                }
                else {
                    # else is label
                    # is label
                    $mynum = $params[0];
                    # reportError($i, "DC.W parameter ($mynum) is not a valid symbol");
                    ($junk, $mynum) = split(/#/, $mynum, 2);
                    if (isSymbol($mynum) ) {
                        $mynum = $SYMBOLS{$mynum};
                        if ($mynum > 0) {
                            $result = "";
                            for ($count = 0; $count < $mynum; $count++) {
                                $result .= "0000";
                            }
                            saveLine("$result", $i, $ADDRESS);
            
                            $size = dataByteSize($result);
                            $size = $size / 2;
                            $ADDRESS = $ADDRESS + $size;
                        }
                        else {
                            reportError($i, 1, "DS.W parameter must be greater than zero");                        
                        }
                    }
                    else {
                        $myerror = "DS.W parameter is not a valid symbol.\n" .
                                   "It must either be a number, or a valid label";
                        reportError($i, 1, $myerror);
                        $mynum = "";
                    }                    
                }
            }                        
        }
        elsif ($instruction =~ /^ORG$/i) {
            saveLine("", $i, $ADDRESS);
            if ($#params != 0) {
                reportError($i, 1, "1 ORG parameter expected, which must be a number");
            }
            else {
                if (isNumber($params[0])) {
                    # set the address to the new value
                    $ADDRESS = hexNumber($params[0], $i);
                    $ADDRESS = hex($ADDRESS);
                    $mynum = formatNumber($ADDRESS, 4);
                    saveLine("org \$$mynum", $i, $ADDRESS);
                }
                else {
                    $myerror = "Invalid ORG parameter \'$params[0]\'\n" .
                               "It must be a number";
                    reportError($i, 1, $myerror);
                }
            }                        
        }
        elsif ($instruction =~ /^MCO$/i) {
            saveLine("", $i, $ADDRESS);
            if ($#params != 0) {
                $myerror = "1 MCO parameter expected, which must be a filename\n" .
                           "in double quotes";                
                reportError($i, 1, $myerror); 

            }
            else {
                if ($params[0] =~/^\"(.*)\"$/i) {
                    ($junk, $mco, $junk2) = split("\"", $params[0], 3);
                    readMCO($mco);  # read the MCO file - establish %INSTRUCTIONS
                }
                else {
                    $myerror = "Invalid MCO parameter \'$params[0]\'\n" .
                               "It must be a filename in double quotes";
                    reportError($i, 1, $myerror);
                }
            }                                
        }
        elsif ($instruction =~ /^LPC$/i) {
            saveLine("", $i, $ADDRESS);
            if ($#params != 0) {
                reportError($i, 1, "1 LPC parameter expected, which must be a number");
            }
            else {
                if (isNumber($params[0])) {
                    # set the address to the new value
                    $ADDRESS = hexNumber($params[0], $i);
                    $ADDRESS = hex($ADDRESS);
                    $mynum = formatNumber($ADDRESS, 4);
                    saveLine("lpc \$$mynum", $i, $ADDRESS);
                }
                else {
                    $myerror = "Invalid LPC parameter \'$params[0]\'\n" .
                               "It must be a number";
                    reportError($i, 1, $myerror);
                }
            }                        
        }
        elsif ($instruction =~ /^EQU$/i) {
            saveLine("", $i, $ADDRESS);
            if ($#params != 0) {
                reportError($i, 1, "1 EQU parameter expected, which must be a number");
            }
            else {
                if (isNumber($params[0])) {
                    # set the symbol to the new value
            
                    $mynum = hexNumber($params[0],$i);
                    $mynum = hex($mynum);
                    $SYMBOLS{$symbol} = $mynum;
                    saveLine("", $i, $ADDRESS);
                }
                else {
                    $myerror = "Invalid EQU parameter \'$params[0]\'\n" .
                               "It must be a number";
                    reportError($i, 1, $myerror);
                }
            }        
        
        }        
        else {
            #
            # must have been an instruction
            #
            ($retval, $data) = getMachineCode($instruction, $params, $i);
            
            if ($retval) {
                # do something with the machine code data
                saveLine($data, $i, $ADDRESS);
        
                # now update the address
                $size = dataByteSize($data);
                $size = $size / 2;
                $ADDRESS = $ADDRESS + $size;
            }
            else {
                saveLine("", $i, $ADDRESS);
                $myerror = "This instruction does not match any instruction\n" .
                           "in the instruction set.  Check that the mnemonic\n" .
                           "is not mis-spelt and that the parameters are valid\n" .
                           "for your required instruction.";
                reportError($i, 1, $myerror);
                
            }
        }
    }
    else {
        # it's a nothing line
        saveLine("", $i, $ADDRESS);
    }

} # end for


if (($displayOutput ne "code")&&($displayOutput ne "printout")) {
    printIntro($asp, $mco, $displayOutput) ;
}

if ($displayOutput eq "debug") {
    displayObjectCode("Initial Pass");
}

updateObjectCode();


if ($displayOutput eq "printout") {
    displayListing("$displayOutput");
}
elsif ($displayOutput eq "code") {
    displayListing("$displayOutput");
}
elsif ($displayOutput eq "debug") {
    displayObjectCode("Second Pass");
    displayListing("$displayOutput");
    displaySymbols();
    displayMnemonics();
    displayErrors();
}
else { # default
    displayListing("$displayOutput");
    displaySymbols();
}

if (($displayOutput ne "code")&&($displayOutput ne "printout")) {
    sendOutput("# Assembler completed ");
    if ($ERRORS) {
        sendOutput("with $ERRORS error(s)\n");
    }
    else {
        sendOutput("successfully.\n");
    }
    sendOutput("#\n");
}    

if ($ERRORS) {
    print STDERR "# $ERRORS error(s)\n";
}
else {
    print STDERR "# Success\n";
}


if ($opt_o) { 
    close(OUTFILE);
}
exit;

#
#-----------------------------------------------------------------------------
#

sub printLine {
    sendOutput("#");
    sendOutput("-"x79);
    sendOutput("\n");
}

#
#-----------------------------------------------------------------------------
#

sub normaliseChars {
    my($char) = @_;
    $char =~ s/\\,/\x01/g; # use x01 as commas in strings
    $char =~ s/\\'/\x02/g; # use x02 as ' in strings

    my $result;
    my @chars = split(/\,/, $char);
    foreach $char (@chars) {
        $char =~ s/^\s+//;
        $char =~ s/\s+$//;

        if ($char =~ /^'(.*)'$/i) {
            $char = $1;
            $char =~ s/\\n/\n/g; # Newline
            $char =~ s/\\r/\r/g; # CR
            $char =~ s/\\a/\a/g; # Alarm (Beep)
            $char =~ s/\\f/,/g; # Formfeed used for commas
            $char =~ s/\\t/\t/g; # Tab

            $char =~ s/\x01/,/g; # put commas back in strings
            $char =~ s/\x02/'/g; # put ' back in strings


            my (@array) = unpack("C*", $char);
            foreach $char(@array) {
                $char = sprintf("%2.2x", $char);
                #sendOutput STDERR "XX[$char,]";
                $result .= "$char,";
            }
        }
        else {
            $result .= "$char,"
        }
    }

    return($result);
}

#
#-----------------------------------------------------------------------------
#

sub printIntro {
    my($asm, $mco, $type) = @_;

    printLine();
    sendOutput("#\n");

    sendOutput("# $copyrightString\n");
    sendOutput("# $versionString ($versionDate)\n");
    sendOutput("#\n");
    sendOutput("# Assembler input : $asm\n");
    sendOutput("# Instruction set : $mco\n");    
    sendOutput("# Assembler mode  : $type\n");
    sendOutput("#\n");

}

#
#-----------------------------------------------------------------------------
#

sub getMachineCode {
    #  in  - 1=instruction, 2=params
    #  out - 1 = retval 0=fail, 1=ok
    #        2 = data

    my ($instruction, $params, $i) = @_;

    my($validInstruction, $value, $actualInstruction, $actualParams);
    my(@actualParams, $p, $codeParam, $definedParam, $expression, %Matching);
    my($e1, $e2, $paramMatch, $opcode, $returnVal, $operand, $x, $newoperand);

    #
    # see if the instruction is a valid one
    #
    %Matching = ();
    $validInstruction = 0;
    
    
    foreach $value (keys %INSTRUCTIONS) {
        if ($INSTRUCTIONS{$value} =~ /^\Q$instruction\E\b/i) {
            $validInstruction = 1;
            $Matching{$value} = $INSTRUCTIONS{$value};
        }
    }

    if ($validInstruction) {
        #
        # we have a valid menomonic, now we have to check if we have 
        # the right params
        #    
        
        foreach $value (values %Matching) {
        
            $paramMatch = -1;
            $operand = "";
        
        
            if ($value =~ /^\Q$instruction\E\b/i) {
                #
                # this instruction matches
                # so look at the parameters in turn
                $value =~ s/^\s+//;  
                ($actualInstruction, $actualParams) = split(/\s/, $value);
                $actualParams =~ s/^\s+//;  
                $actualParams =~ s/\s+$//; # remove leading and trailing whitespace
                @actualParams = split(/\,/, $actualParams);
        
                if ($#actualParams == $#params) {
                    #
                    # same number of parameters, so worth checking
                    #
            
                    if ($#actualParams == -1) {
                        # instruction has no parameters, so use this instruction
               
                        $value =~ s/^\s+//;  
                        $value =~ s/\s+$//;
                        $opcode = formatNumber($MNEMONICS{$value});
                        # POSITIVE IDENT [$value][$opcode]
                        if ($FORCE_32BIT) {
                            $opcode = $opcode . ":000000";
                        }
                        else {
                            $opcode = $opcode . ":00";
                        }
                        return(1, $opcode);
                    }
                    else {
                        # one or more parameters
                        #
                        #
                        # compare each param in here, if all match then we
                        # have the correct instruction
                        #
    
                        for($p = 0; $p <= $#actualParams; $p++) {
            
                            # for each parameter in the instruction, see if we have
                            # a match
                
                            $definedParam  = $actualParams[$p];
                                
                            # we need to figure out if $definedParam contains a
                            # a data/dis value, an address or a register

                            if (isRegister($definedParam, $params[$p]) ) {
                                $paramMatch++;
                            }
                            elsif (isType($definedParam, $params[$p], "DATA", 8) ) {
                                $newoperand = getOperand($definedParam, $params[$p], $i, "DATA", 8);
                                $operand = updateOperand($operand, $newoperand);
                                $paramMatch++;
                            }
                            elsif (isType($definedParam, $params[$p], "DATA", 16) ) {
                                $newoperand = getOperand($definedParam, $params[$p], $i, "DATA", 16);
                                $operand = updateOperand($operand, $newoperand);
                                $paramMatch++;
                            }
                            elsif (isType($definedParam, $params[$p], "ADDR", 8) ) {
                                $newoperand = getOperand($definedParam, $params[$p], $i, "ADDR", 8);
                                $operand = updateOperand($operand, $newoperand);
                                $paramMatch++;
                            }
                            elsif (isType($definedParam, $params[$p], "ADDR", 16) ) {
                                $newoperand = getOperand($definedParam, $params[$p], $i, "ADDR", 16);
                                $operand = updateOperand($operand, $newoperand);
                                $paramMatch++;
                            }
                            elsif (isType($definedParam, $params[$p], "DIS", 8) ) {
                                # TEMP
                                $newoperand = getOperand($definedParam, $params[$p], $i, "DIS", 8);
                                $operand = updateOperand($operand, $newoperand);
                                $paramMatch++;
                            }
                            elsif (isType($definedParam, $params[$p], "DIS", 16) ) {
                                # TEMP
                                $newoperand = getOperand($definedParam, $params[$p], $i, "DIS", 16);
                                $operand = updateOperand($operand, $newoperand);
                                $paramMatch++;
                            }
                           
                        } # end of for-loop
            
                        if ($paramMatch == $#actualParams) {
                            $value =~ s/^\s+//;  
                            $value =~ s/\s+$//;
                            $opcode = formatNumber($MNEMONICS{$value});
                              
                            $operand = completeOperand($operand);
                            $returnVal = $opcode . $operand;

                            return(1, $returnVal);
                            last;
                        }
                    }
                }
                else {
                    # different number of parameters, so do nothing
                }
            }
        }

        return 0;
    }
    else {
        return 0;
    }
}

#
#-----------------------------------------------------------------------------
#

sub updateOperand {

    # this updates the operand, such that words can only be added
    # on word boundaries.  bytes can be added no matter what
    # 
    my($op, $new) = @_;

    my $opsize = dataByteSize($op) + 1;  # 1 for the opcode
    my $newsize = dataByteSize($new);
    
    # remember this is just the operands, not the opcode too
    # (but we've accounted for it)
    if ( ($opsize % 2)==1 ) {
       if ( ($newsize % 2) == 0 ) {
           # and new value to add on is word boundary
           # so prefix with 00
           $new = "00" . $new;
       }
    }

    $op = $op . ":" .$new;

    return("$op");

}

#
#-----------------------------------------------------------------------------
#

sub completeOperand {

    my($op,) = @_;

    my($size) = dataByteSize($op) + 1;
    
    if ( (($size %2)==1) && ($FORCE_32BIT) ) {
        $op = $op .":000000";
    }
    elsif ( ($size % 2) == 1) {
        $op = $op . ":00";
    }

    return("$op");
}

#
#-----------------------------------------------------------------------------
#

sub dataByteSize {

    # work out how big the operand is, in bytes
    my($op) = @_;
    my($part, @parts, $c);
    my($retval) = 0;

    @parts = split(":", $op);
    
    foreach $part (@parts) {
        if ( $part =~ /^\s*$/i ) {
            # ignore, it's empty
        }
        elsif ($part =~ /^[0-9a-f]+$/i) {
            # it's a hex value
            $c = length($part);
            $c = $c / 2;
            $retval = $retval + $c;
        }
        elsif ($part =~ /^\w*\!(\d+)[AR]$/i) {
            $c = $1;
            if ($c == 8) {
                $retval = $retval + 1;
            }
            elsif ($c == 16) {
                $retval = $retval + 2;
            }
            else {
                # report bug
                fatalError("dataByteSize had an 8/16 bit error");
            }
        }
        else {
            # report bug
            fatalError("operandSize had an error with [$op]");
        }
    }

    return $retval;
}

#
#-----------------------------------------------------------------------------
#

sub updateObjectCode {

    my($i, $val, $address, $op, $part, @parts, $entry, $c, $newcode, $type);
    my($symbol, $num, $myadd);

    for($i = 1; $i <= $#ASM; $i++) {
        $val = $ASM[$i];
    
        $newcode = "";
    
        ($address, $op) = split( /!/, $val, 2);

        if ($op =~ /^[0-9a-f:]+/i) {
            @parts = split(":", $op);
    
            foreach $part (@parts) {
                if ( $part =~ /^\s*$/i ) {
                    # ignore, it's empty
                }
                elsif ($part =~ /^[0-9a-f]+$/i) {
                    # it's a hex value
                    $newcode = $newcode . "$part";
                }
                elsif ($part =~ /^(\w*)\!(\d+)([AR])$/i) {
                    $symbol = $1;
                    $c = $2;
                    $type = $3;
                    if ($type eq "A") {
                        # absolute
                        if ($c == 8) {
                            if (!(is8bit($SYMBOLS{$symbol}))) {
                                reportError($i, 1, "Symbol value is not 8 bit");
                            }
                            $num = formatNumber($SYMBOLS{$symbol}, 2);
                            $newcode = $newcode . $num;
                        }
                        elsif ($c == 16) {
                            if (!(is16bit($SYMBOLS{$symbol}))) {
                                reportError($i, 1, "Symbol value is not 16 bit");
                            }
                            $num = formatNumber($SYMBOLS{$symbol}, 4);
                            $newcode = $newcode . $num;               
                        }
                        else {
                            # report bug
                            fatalError("updateObjectCode had an 8/16 bit error");
                        }
                    }
                    elsif ($type eq "R") {
                        if ($c == 8) {
                            $myadd = hex($address);
                            $num =  $SYMBOLS{$symbol} - $myadd;
               
                            $num--;
               
                            if ($num < 0) {
                                $num = 256 + $num;
                                if (!(is8bit($num))) {
                                    reportError($i, 1, "Symbol value is not 8 bit");
                                }
                                $num = formatNumber($num, 2);
                                $newcode = $newcode . $num;               
                            }
                            else {
                                if (!(is8bit($num))) {
                                    reportError($i, 1, "Symbol value is not 8 bit");
                                }
                                $num = formatNumber($num, 2);
                                $newcode = $newcode . $num;               
                            }
           
                        }
                        elsif ($c == 16) {

                            $myadd = hex($address);
                            
                            $num =  $SYMBOLS{$symbol} - $myadd;
               
                            $num--;
               
                            if ($num < 0) {
                                $num = 65535 + $num;
                                if (!(is16bit($num))) {
                                    reportError($i, 1, "Symbol value is not 16 bit");
                                }
                                $num = formatNumber($num, 4);
                                $newcode = $newcode . $num;               
                            }
                            else {
                                $num--; # remember, we have to take off an extra location
                                if (!(is8bit($num))) {
                                    reportError($i, 1, "Symbol value is not 16 bit");
                                }
                                $num = formatNumber($num, 4);
                                $newcode = $newcode . $num;               
                            }


                        }
                        else {
                            # report bug
                            fatalError("updateObjectCode had an 8/16 bit error");
                        }               
                    }
                    else {
                        # report bug
                        fatalError("updateObjectCode had an A/R error");
                    }
                }
            }
        
            $ASM[$i] = "$address\!$newcode";
        }
    }
}

#
#-----------------------------------------------------------------------------
#

sub getOperand {
    my ($pattern, $value, $i, $type, $size) = @_;
   
    my($expression, $start, $finish, $num);
    my($junk);
   
    if ( ($type ne "DATA") && ($type ne "ADDR") && ($type ne "DIS") ) {
        # report bug
        fatalError("get${type} incorrect type");
    }
   
    # 
    # check to see if the pattern is a data pattern, if it
    # isn't return false, otherwise to check to see if the
    # value is a correct data value.  If it isn't, return false
    # else return true.
    #
   
    if ($size == 8) {
        $expression = expression(@DATA8) if ($type eq "DATA");                
        $expression = expression(@ADDR8) if ($type eq "ADDR");                
        $expression = expression(@DIS8) if ($type eq "DIS");                
    }
    elsif ($size == 16) {
        $expression = expression(@DATA16) if ($type eq "DATA");                
        $expression = expression(@ADDR16) if ($type eq "ADDR");                
        $expression = expression(@DIS16) if ($type eq "DIS");                
    }
    else {
        # report bug
        fatalError("get${type} size error");
    }
   
    my($retval) = "";

    if ($pattern =~ /^(.*)\b($expression)\b(.*)$/i) {
        # so, we may be dealing with a valid data value
        # is it a label
        $start = $1;
        $finish = $3;
       
        $start  = "\Q$start\E";
        $finish = "\Q$finish\E";

        if ($value =~ /^${start}(.*)${finish}$/i) {
            $num = $1;
       
            if (isNumber($num, "strict") ) {
       
                $num = hexNumber($num, $i);

                if ($size == 8) {
                    if (is8bit($num) ) {
                        $num = resize($num, 8);
                    }
                    else {
                        $myerror = "A parameter was used that does not fit\n" .
                                   "in the required 8-bits ($num).";
                        reportError($i, 1, $myerror);
                        $num = "00";
                    }
                }
                elsif ($size == 16) {
                    if (is16bit($num) ) {
                        $num = resize($num, 16);
                    }
                    else {
                        $myerror = "A parameter was used that does not fit\n" .
                                   "in the required 16-bits ($num).";
                        reportError($i, 1, $myerror);
                        $num = "0000";
                    }
                }
           
                $retval =  "$num";
            }
            else {
                if (isSymbol($num) ) {
                    $retval = ":$num!8"  if ($size == 8);
                    $retval = ":$num!16" if ($size == 16);
           
                    if ($type eq "DIS") {
                        $retval = $retval . "R"; # for 'relative'
                    }
                    else {
                        $retval = $retval . "A"; # for 'absolute'
                    }
                }
            }
        }
    }

    return($retval);
}

#
#-----------------------------------------------------------------------------
#

sub is8bit {
    # this function takes a decimal input

    my($num) = @_;
    my $testvalue = $num;
    my($x) = 256;

    if ( ($testvalue >= 0) && ($testvalue < $x) ) {
        return 1;
    }
    else {
        return 0;
    }
}

#
#-----------------------------------------------------------------------------
#

sub is16bit {
    # this function takes a decimal input

    my($num) = @_;
    my $testvalue = $num;
    my($x) = 256 * 256;
    
    if ( ($testvalue >= 0) && ($testvalue <= $x) ) {
        return 1;
    }
    else {
        return 0;
    }
}

#
#-----------------------------------------------------------------------------
#

sub resize {
    # takes a hex value, and resizes it to $size 
    my($num, $size) = @_;
    my($retval) = $num;
    
    if ( ($size != 8) && ($size != 16) ) {
        # report bug
        fatalError("resize size error");
    }
    
    $size = 2 if ($size == 8);
    $size = 4 if ($size == 16);
    
    if (length($num) < $size) {
        while (length($num) < $size) {
            $num = "0" . $num;
        }
    }
    
    return "$num";
}

#
#-----------------------------------------------------------------------------
#

sub isType {
    my ($pattern, $value, $type, $size) = @_;
   
    my($expression, $start, $finish, $num);
    my($retval) = 0;

    if ( ($type ne "DATA") && ($type ne "ADDR") && ($type ne "DIS") ) {
        # report bug
        fatalError("is${type} incorrect type");
    }
   
    # 
    # check to see if the pattern is a data pattern, if it
    # isn't return false, otherwise to check to see if the
    # value is a correct data value.  If it isn't, return false
    # else return true.
    #

    if ($size == 8) {
        $expression = expression(@DATA8) if ($type eq "DATA");                
        $expression = expression(@ADDR8) if ($type eq "ADDR");                
        $expression = expression(@DIS8) if ($type eq "DIS");                
    }
    elsif ($size == 16) {
        $expression = expression(@DATA16) if ($type eq "DATA");                
        $expression = expression(@ADDR16) if ($type eq "ADDR");                
        $expression = expression(@DIS16) if ($type eq "DIS");                
    }
    else {
        # report bug
        fatalError("is${type} size error");
    }
   
    if ($pattern =~ /^(.*)\b($expression)\b(.*)$/i) {
        # so, we may be dealing with a valid data value
        # is it a label
        $start = $1;
        $finish = $3;
       
        $start  = "\Q$start\E";
        $finish = "\Q$finish\E";

        if ($value =~ /^${start}(.*)${finish}$/i) {
            $num = $1;
            if (isNumber($num, "strict") ) {
                $retval = 1;
            }
            else {
                if (isSymbol($num) ) {
                    $retval = 1;
                }
            }
        }
    }
    return($retval);
}


#
#-----------------------------------------------------------------------------
#

sub isRegister {
    my ($pattern, $value) = @_;
   
    my($expression, $start, $finish, $reg);
    my($retval) = 0;
   
    # 
    # check to see if the pattern is a data pattern, if it
    # isn't return false, otherwise to check to see if the
    # value is a correct data value.  If it isn't, return false
    # else return true.
    #

    $expression = expression(@REGISTERS);                

    if ($pattern =~ /^(.*)\b($expression)\b(.*)$/i) {
        # so, we may be dealing with a valid register value
              
        $value  = "\Q$value\E";

        if ($pattern =~ /^${value}$/i) {
            $retval = 1;
        }
    }

    return($retval);
}


#
#-----------------------------------------------------------------------------
#

sub expression {

    my @val = @_;
    my($val, $exp);

    if ($#val == -1) {
        # report bug
        fatalError("Internal Error in \'expression\'");
    }

    foreach $val (@val) {
        $exp .= "\Q$val\E|";
    }
    chop $exp;
   
    return $exp;
}

#
#-----------------------------------------------------------------------------
#

sub reportError {
    #
    # add the error to the error structure
    #
    my ($i, $report_error, $message) = @_;

    if (! $i) {
        $i = 0;
    }

    if ($ERRORS[$i]) {
        # we already have at least one error on that line
        $message = $ERRORS[$i] . "\0" . $message;
    }
#    else {
        $ERRORS[$i] = $message;
#    }
    
    if ($report_error) {
        $ERRORS++;
    }
}


#
#-----------------------------------------------------------------------------
#

sub fatalError {
    #
    # report the error
    #
    my ($message) = @_;
    sendOutput("# Fatal Error : $message\n");
    sendOutput("# Sorry, had to stop because I couldn't recover from this.\n");
    sendOutput("# If you think you've found a bug then please send the assembly\n");
    sendOutput("# file to me at $emailString, and I'll check it out.\n");
    print STDERR "# Fatal Error\n";
    exit -1;
}


sub fileError {
    #
    # report the fileError
    #
    my ($message) = @_;
    sendOutput("# File Error : $message\n");
    sendOutput("# File Error : The filename is either missing or mis-spelt.\n");
    sendOutput("# File Error : Please correct and then run the assembler once more\n");
    print STDERR "# File Error\n";
    exit -1;
}


#
#-----------------------------------------------------------------------------
#
sub loadSymbols {
    
    my($i, $line, $origline, $symbol);

    for($i = 1; $i <= $#SOURCE; $i++) {
        # look at each source line in turn
    
        $origline = $SOURCE[$i];
        ($line) = split(/\*/, $origline); # remove any comments

        if ($line =~ /^\s*$/) {
            # it's a blank line, so do nothing
        }
        elsif ($line =~ /^(\w+)\b(.*)$/i) {
            # we have found a symbol at $1, $2 is the instruction and data
            $symbol = $1;
            if (defined ($SYMBOLS{$symbol})) {
                # the symbol has already been defined
                $myerror = "The symbol \'$1\' is duplicated.\n" .
                           "Make sure all labels are unique.";
                reportError($i, 1, $myerror);
            }
            else {
                # new symbol, so enter it into the hash array
                $SYMBOLS{$symbol} = $i; # set it to the line number
            }
        }
        elsif ($line =~ /^\s+(.*)$/i) {
            # there is no symbol on this line
            # $1 is the instruction and data
        }
        else {
            reportError($i, 1, "Unknown value found while parsing symbols.");
        }
    }
}

#
#-----------------------------------------------------------------------------
#

sub formatNumber {

# will format the given decimal value, as either 
# a 4 or 2 digit hex number

    my($num, $size) = @_;
    my($result);

    if (! $size) { $size = 2; }

    #
    # $num is decimal, so convert to hex
    #
    $result = sprintf("%${size}.${size}x", $num);

    return $result;

}

#
#-----------------------------------------------------------------------------
#
sub isNumber() {
    #
    # to have a leading $,%,@,^
    # or assumbed to be hex, if no match
    # then invalid, 
    #
    # if format is "strict", then
    # always has to have a leading $,%,@,^ otherwise
    # invalid
    #
    
    my ($val, $format) = @_;
    
    if ($format eq "strict") {
        if ( 
            ($val =~ /^\%[0-1]+$/i)         ||      # binary
            ($val =~ /^\@[0-7]+$/i)         ||      # octal
            ($val =~ /^\^-{0,1}[0-9]+$/i)   ||      # decimal
            ($val =~ /^\$[0-9a-f]+$/i)              # hex value 
        ) { return 1; } else { return 0; }
    }
    else {
        if ( 
            ($val =~ /^\%[0-1]+$/i)         ||      # binary
            ($val =~ /^\@[0-7]+$/i)         ||      # octal
            ($val =~ /^\^-{0,1}[0-9]+$/i)   ||      # decimal
            ($val =~ /^\${0,1}[0-9a-f]+$/i)         # hex value 
        ) { return 1; } else { return 0; }
    }

}

#
#-----------------------------------------------------------------------------
#

sub hexNumber() {
    #
    # takes a number with a leading $,%,@,^ (otherwise)
    # assumed to be hex, and will return value as a string in hex.
    
    my($val, $i) = @_;
    my($retval);

    if ($val =~ /^\%[0-1]+$/i) {
        # binary
        $val = substr($val, 1, length($val) ); # get rid of leading char
        $retval = unpack("N", pack("B32", substr("0" x 32 . $val, -32) ) );
        # now in decimal
        $retval = sprintf("%x", $retval);
    }
    elsif ($val =~ /^\@[0-7]+$/i) {
        # octal
        $val = substr($val, 1, length($val) ); # get rid of leading char
        $val = oct($val);
        # now in decimal
        $retval = sprintf("%x", $val);
    }
    elsif ($val =~ /^\^-{0,1}[0-9]+$/i) { 
        # decimal
        #
        # this has been updated to cope with negative values
        # (mostly because the jcc compiler spits them out)
        #
        $val = substr($val, 1, length($val) ); # get rid of leading char
        if ($val < 0) {
            $val = 65536 + $val; # to give a 2's comp value
        }
        $retval = sprintf("%x", $val);
    }
    elsif ($val =~ /^\$[0-9a-f]+$/i) {
        # hex value, strict
        $val = substr($val, 1, length($val) ); # get rid of leading char
        $retval = $val;
    }
    elsif ($val =~ /^\${0,1}[0-9a-f]+$/i) {
        # hex value
        $retval = $val;
    }
    else {
        $myerror = "Congratulations, you might have found a bug!\n" .
                   "Check that you have used the correct instruction\n" .
                   "set for your program just in case - as some bizarre\n" .
                   "but normal situations can cause this failure.\n" .
                   "If you still think that this problem is mine and\n" .
                   "not yours, then please send a debug output (and\n" .
                   "original source file) of this program to me at\n" .
                   "$emailString. Thanks!\n" .
                   "\n" .
                   "The bug - HexNumber failure swallowing [$val]."; 
        reportError($i, 1, $myerror);
   }

    return "$retval";
}

#
#-----------------------------------------------------------------------------
#

sub isSymbol {

    my ($token) = @_; 
    my ($directive, $symbol, $found);
    
    $found = 0;
    foreach $directive (keys %SYMBOLS) {
        if ($token =~ /^$directive$/i) {
            $found = 1;
            last;
        }
    }
    if ($found) { return 1; } else { return 0; }
}

#
#-----------------------------------------------------------------------------
#
sub analyseLine {
   #
   # takes line number of source file as input
   #
   #
   # outputs 
   # retval = 0 if nothing found, 1 if info to pass back
   # instruction - the instruction found
   # params - any parameters for the instruction
   # symbol - symbol defined on line


    my $i = $_[0]; # line number of source code
    my $origline = $SOURCE[$i];

    my($line, $symbol, $instruction, $parameters);

    ($line) = split(/\*/, $origline); # remove any comments

    if ($line =~ /^\s*$/) {
        # it's a blank line, so do nothing
        return 0;
    }
    elsif ($line =~ /^(\w+)\s*$/i) {
        # a symbol on its own
        $symbol = $1;
        $SYMBOLS{$symbol} = $ADDRESS;
        return 0;
    }    
    elsif ($line =~ /^(\w+)\b(.*)$/i) {
        # we have found a symbol at $1, $2 is the instruction and data
        $symbol = $1;
        $instruction = $2;
    
        #
        # update the symbol table here with a new value for $symbol
        # with the current address
        $SYMBOLS{$symbol} = $ADDRESS;
    }
    elsif ($line =~ /^\s+(.*)$/i) {
        # there is no symbol on this line
        # $1 is the instruction and data
        $instruction = $1;
    }
    else {
        reportError($i, 1, "Unknown value found while parsing symbols");
    }
    
    
    $instruction =~ s/^\s+//;  $instruction =~ s/\s+$//; # remove leading and trailing whitespace
     
    ($instruction, $params) = split(/\s/, $instruction, 2);
     
    if (defined $params) {
        $params =~ s/^\s+//;  $params =~ s/\s+$//; # remove leading and trailing whitespace
    }
    return(1, $instruction, $params, $symbol);
}

#
#-----------------------------------------------------------------------------
#
sub countParams {
   my($params) = $_[0];
   my(@params) = ();

   return 0 if (! $params);
   @params = split(/\,/, $params);
   
   my $count = $#params + 1;
   return $count;
}

#
#-----------------------------------------------------------------------------
#
sub readASM {
    #
    # one level USE statements implemented
    #

    my ($asm) = $_[0];
    my($count, $line);
    my $useline;

    if ($asm) {
        open(ASM, "$asm") || fileError("Failed to open [$asm]");

        $count = 0;

        while($line = <ASM>) {
            $count++;
            chomp $line;
            my $linecheck = $line;
            $linecheck =~ s/^\s+//;
            $linecheck =~ s/\s+$//;
            if ($linecheck =~ /^use\s+\"(.*)\"$/i) {
                #
                # we need to perform an include
                #
                $count--;
                my $include = $1;
                
                # check if the library file is in the current directory, 
                # otherwise load it from the JASM directory
                if (-e "$include") {
                    # it is in this directory
                }
                else {
                    if ($ENV{JASP}) {
                        $include = $ENV{JASP} . $include;
                    }
                }
                
                open(INCLUDE, "$include") || fileError("Failed to open [$include]");
                while ($useline = <INCLUDE>) {
                    $count++;
                    chomp $useline;
                    $SOURCE[$count] = $useline;
                }
                close(INCLUDE);
            }
            else {
                $SOURCE[$count] = $line;
            }
        }
        close(ASM);
    }
    else {
        fileError("No assembler file given");
    }
}

#
#-----------------------------------------------------------------------------
#
sub displayObjectCode {
    my ($i, $entry, $address, $val, $opcode, $tag);

    my $message = $_[0];
    sendOutput("#\n");
    printLine();
    sendOutput("#\n");
    sendOutput("# Object Code - $message\n#\n");


    for($i = 1; $i <= $#ASM; $i++) {
        $val = $ASM[$i];
    
        ($address, $entry) = split( /!/, $val, 2);

        $address = uc($address);
        $entry   = uc($entry);

        if ( !($entry =~ /^[\s]*$/i ) ) {
            # if the entry isn't just whitespace, do
            # something with it
    
            sendOutput("# $address [$entry]");

            if ($entry =~ /^[0-9a-f][0-9a-f]/i) {
                # if entry begins with 2 hex chars
                # assume that we have an opcode
        
                $opcode = substr($entry, 0, 2);
    
                # sendOutput "[$opcode]";
        
                $tag = getTag($opcode, "hex");
                sendOutput(" $tag");
            }
            sendOutput("\n");    
        }
    }
}

#
#-----------------------------------------------------------------------------
#
sub displayListing {
    # provides the final listing that
    # can be loaded into ASP.
    # 

    my ($listing) = @_;

    if (($listing ne "code")&&($listing ne "printout")) {
        printLine();
        sendOutput("#\n# Assembler Listing\n#\n");
        printLine();
    }

    my ($i, $entry, $address, $length, $val);
    $length = $#ASM;
    
    for($i = 1; $i <= $length; $i++) {

        $val = $ASM[$i];
    
        $val = "" if (! $val);
        ($address, $entry) = split( /!/, $val, 2);
        $entry = " " if (! $entry);
        if (! $address) {
            $address = "    "; 
        }
        displayLine($i, $entry, $address, $listing);
    }
}

#
#-----------------------------------------------------------------------------
#
sub displayTest {
     my ($i, $length, $val);
    $length = $#ASM;
    for($i = 1; $i <= $length; $i++) {
        $val = $ASM[$i];
        $val = "" if (! $val);
        sendOutput("OBJECT [$i][$val]\n");
    }
}

#
#-----------------------------------------------------------------------------
#
sub displaySubLine {

    my($entry, $address, $tag, $line, $listing) = @_;
    my($wholeline);
    my $asplinelimit = 250 - 2; # -2 is for CR/LF
    my($limit) = 10;
    my($taglimit) = 20;

    $tag = updateString($tag, $taglimit);
 
    $entry = updateString($entry, $limit);
    
    $entry   = uc($entry);
    $address = uc($address);

    if (($listing eq "default")||($listing eq "printout")) {
        $wholeline = "$entry # $address # $line";
    }
    elsif ($listing eq "debug") {
        $wholeline = "$entry # $address # $tag # $line";
    }
    elsif ($listing eq "code") {
        $wholeline = "$entry";
    }
    else {
        # report bug
        fatalError("unknown listing type [$listing] in displayLine");
    }
    
    $wholeline = substr($wholeline, 0, $asplinelimit);
    
    if ($wholeline =~ /^\s*$/i) {
        # then do nothing
    }
    else {
        $wholeline =~ s/\s+$//; # get rid of trailing spaces
        sendOutput("$wholeline\n");
    }
}

#
#-----------------------------------------------------------------------------
#
sub displayLine {


    my($i, $entry, $address, $listing) = @_;
    my($notDone) = 1;

    my($newentry, @params, $x, $sym, $size, $symval, @myerrors, @myerrorlines);
    my($length, $count);
    my $wholeline;
    my ($j);
    my($opcode, $tag, $number);
    
    $tag = " ";
    my $line = $SOURCE[$i];

    if ($entry =~ /^[0-9a-f][0-9a-f]/i) {

        $length = dataByteSize($entry);
        $count = 0;
        while ($length > 0) {
    
            # sendOutput a line
            $count++;
    
            if ($count == 1) {
                # it's the start of the line
                $opcode = substr($entry, 0, 2);
                $tag = getTag($opcode, "hex");
                displaySubLine(substr($entry,0,4), $address, $tag, $line, $listing);        
            }
            else {
                # it's just data
                displaySubLine(substr($entry,0,4), "    ", "", "", $listing);        
            }

            $entry = substr($entry, 4);
            $length = dataByteSize($entry);
    
        }
    }
    else {
        displaySubLine($entry, $address, $tag, $line, $listing);        
    }
    
    if ( $ERRORS[$i] ) {
        @myerrors = split(/\0/, $ERRORS[$i]);
        foreach (@myerrors) {
            # sendOutput("# Error - $_\n");
            sendOutput("#\n");
            
            @myerrorlines = split(/\n/, $_);
            for($j = 0; $j <= $#myerrorlines; $j++) {
                if ($j == 0) {
                    sendOutput("# Error -  $myerrorlines[$j]\n");
                }
                else {
                    sendOutput("#          $myerrorlines[$j]\n");
                }
            }
        }
        sendOutput("#\n");
    }
}

#
#-----------------------------------------------------------------------------
#
sub updateString {

    my($str, $limit) = @_;
    if (length($str) < $limit) {
        while (length($str) < $limit) {
            $str .= " ";
        }
    }

    $str = substr($str, 0, $limit);
   
    return($str);
}

#
#-----------------------------------------------------------------------------
#

sub getTag {
    my($num, $base) = @_;

    $base = "hex" if (! $base);

    if ($base eq "hex") {
        # convert base to decimal
        $num = hex($num);
    }
    elsif ($base eq "dec") {
        # assume base is decimal, so do nothing
    }
    else {
        # report bug
        fatalError("incorrect base for getTag");
    }
    
    if (length($num) < 2) {
        while (length($num) < 2) {
            $num = "0" . "$num";
        }
    }

    if ($INSTRUCTIONS{$num}) {
        return($INSTRUCTIONS{$num});
    }
    else {
        return "";
    }

}

#
#-----------------------------------------------------------------------------
#
sub saveLine {
    my($entry, $i, $startaddress) = @_;

    my $myaddress = formatNumber($startaddress, 4);
    $ASM[$i] = "$myaddress!$entry";
}
#
#-----------------------------------------------------------------------------
#
sub displayASM {
    my($limit) = $#SOURCE;
    my($i);
    for($i = 1; $i <= $limit; $i++) {
        sendOutput("# [$SOURCE[$i]\n");
    }
}

#
#-----------------------------------------------------------------------------
#
sub readMCO {

    my ($line, $label, $len, $tag, $opcode);
    my ($mco) = $_[0];
    my (@mcofiles);

    if ($mco) {
    
        # check if we have multiple mco files to read
        @mcofiles = split(/:/, $mco);
        foreach $mco (@mcofiles) {

            # check if the library file is in the current directory, 
            # otherwise load it from the JASM directory
            if (-e "$mco") {
                # it is in this directory
            }
            else {
                if ($ENV{JASP}) {
                    $mco = $ENV{JASP} . $mco;
                }
            }

            open(MCO, "$mco") || fileError("Failed to open [$mco]");
    
            while($line = <MCO>) {
                chop $line;
    
                $line =~ s/^\s+//; # get rid of leading spaces
    
                if ($line =~ /^Force_32bit/i) {
                    $FORCE_32BIT = 1;
                }
                elsif ($line =~ /^Opcode/i) {
                    # woo, it's a new opcode - remember, it may not
                    # be in hex - so convert it.
       
                    ($label, $opcode) = split(/\s/, $line, 2);
                    $opcode =~ s/^\s+//; $opcode =~ s/\s+$//;

                    $opcode = hex($opcode);
       
                    $len = length($opcode);
                    $opcode = "0" . $opcode if ($len == 1);
                }
                elsif ($line =~ /^Mnemonic/i) {
                    ($label, $tag) = split(/\s/, $line, 2);
                    $tag =~ s/^\s+//; $tag =~ s/\s+$//;
                    # now get rid of the speech marks
                    $tag =~ s/\"//g;
       
                    if ($tag) {
                        $INSTRUCTIONS{$opcode} = $tag;
                    }
                    else {
                        # report bug
                        fatalError("TAG for opcode $opcode is not set.");
                    }
                }
                elsif ($line =~ /^Description/i) {
                    # we don't use the DESC tag
                }
                elsif ($line =~ /^#/) {
                    # we don't use comments
                }
                elsif ($line =~ /^\w*$/) {
                    # we don't use blank lines
                }
                else {
                    # and we don't use microinstructions
                }
            }
    
            close(MCO);

            %MNEMONICS = reverse %INSTRUCTIONS;
        }
    }
    else {
        fileError("No instruction set file given");
    }
}

#
#-----------------------------------------------------------------------------
#

sub displaySymbols {
    # display symbol list
    my ($res, $entry);
    
    my($limit) = 40;

    sendOutput("#\n");
    printLine();
    sendOutput("#\n");
    sendOutput("# Symbol table\n#\n");
    foreach (sort (keys %SYMBOLS)) {
        $entry = $_;

        if (length($entry) < $limit) {
            while (length($entry) < $limit) {
                $entry .= ".";
            }
        }
        $res = formatNumber($SYMBOLS{$_} , 4);
        $res = uc($res);
        sendOutput("#  ${entry}.... $res\n");
    }
    sendOutput("#\n");
}

#
#-----------------------------------------------------------------------------
#
sub displayMnemonics {
    # display symbol list
    my ($res, $val);
    
    my($limit) = 40;

    sendOutput("#\n");
    printLine();
    sendOutput("#\n");
    sendOutput("# Instruction table\n#\n");
    foreach (sort (keys %MNEMONICS)) {
    $res = $_;
    if (length($res) < $limit) {
        while (length($res) < $limit) {
        $res .= ".";
        }
    }

    $val = formatNumber($MNEMONICS{$_});

    $val = uc($val);
        sendOutput("#  ${res}.... $val\n");
    }
    sendOutput("#\n");
}


#
#-----------------------------------------------------------------------------
#
sub displayErrors {
    sendOutput("#\n");
    printLine();
    sendOutput("#\n");
    sendOutput("# Error table\n#\n");

    my($limit) = $#SOURCE;
    my($i);
    my($j);
    my(@myerrors);
    my(@myerrorlines);
    for($i = 0; $i <= $limit; $i++) {
    
        if ( $ERRORS[$i] ) {
            @myerrors = split(/\0/, $ERRORS[$i]);
            foreach (@myerrors) {
                while (length($i) < 4) {
                    $i = " $i";
                }
                @myerrorlines = split(/\n/, $_);
                for($j = 0; $j <= $#myerrorlines; $j++) {
                    if ($j == 0) {
                        sendOutput("# $i : $myerrorlines[$j]\n");
                    }
                    else {
                        sendOutput("#        $myerrorlines[$j]\n");
                    }
                }
            }
        }
    }
    sendOutput("#\n");
}

#
#-----------------------------------------------------------------------------
#

sub sendOutput  {
    if ($opt_o) {
        # we are outputting all details to a text file
        print OUTFILE @_;
    }
    else {
       # print to STDOUT
       print @_;
    }
}

#
#-----------------------------------------------------------------------------
#

sub printHelp {

    my $filehandle;
    if ($opt_o) {
        # we are outputting all details to a text file
        $filehandle = *OUTFILE;
    }
    else {
       # print to STDOUT
        $filehandle = *STDOUT;       
    }


    print $filehandle <<"Message";

 NAME
 
   JASP Assembler

 DESCRIPTION 
  
   An assembler for the JASP processor - programs assembled with this
   program can be run in JASPer or ASPEN, or any other JASP clone.

 USAGE
 
    jasm.pl [-m mco][-a asm][-l type][-o filename][-h]

   -m mco      : loads a microcode file (can be multiple files 
                 separated by a ':')
   -a asm      : loads a JASP assembler file
   -l type     : output type can be default|debug|code|printout
   -o filename : send assembler listing to filename listed
   -h          : list this help information.
 
   For example,

    jasm.pl -a fred.txt

   Some dumb systems (like DOS) may require :-

    perl jasm.pl -a fred.txt

   To capture the output in a file, try this :-

    perl jasm.pl -a fred.txt -o fred.jas

   The file fred.jas can then, if no assembly errors are reported, be
   loaded into JASPer or Aspen.

   It is assumed that the microcode file loads successfully in JASPer.

   This is free software and comes with no warranty.

 COPYRIGHT
 
   $copyrightString ($emailString)

 VERSION
 
   $versionString ($versionDate)

Message
    exit(0);
}


