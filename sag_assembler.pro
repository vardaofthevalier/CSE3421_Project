% Define facts

%% INSTRUCTION OPCODES %%

mathop(0, 'NOOP').
mathop(1, 'ADD').
mathop(2, 'SUB').
mathop(3, 'AND').
mathop(4, 'OR').
mathop(5, 'NOT').
mathop(6, 'SLL').
mathop(7, 'SLR').
mathop(8, 'SLT').
mathop(9, 'BEQ').
mathop(10, 'BNEQ').

immop(11, 'ADDI').
immop(12, 'SLTI').
immop(13, 'LOAD').
immop(14, 'STORE').

jumpop(15, 'JUMP').

instruction_set(Return) :-
    findall(MathOp, mathop(_, MathOp), MathOps),
    findall(ImmOp, immop(_, ImmOp), ImmOps),
    findall(JumpOp, jumpop(_, JumpOp), JumpOps),
    append(MathOps, ImmOps, Temp1),
    append(Temp1, JumpOps, Return).
	
%% PSEUDO INSTRUCTIONS %%

set('SET').
clear('CLEAR').
on('ON').
off('OFF').

%% REGISTERS %%

zero_reg(0, 'zero').
reg(1, 'g1').
reg(2, 'g2').
reg(3, 'g3').
reg(4, 'g4').
reg(5, 'g5').
reg(6, 'g6').
reg(7, 'g7').
dev_reg(8, 'd8').
dev_reg(9, 'd9').
dev_reg(10, 'd10').
dev_reg(11, 'd11').
global_pointer(12, 'gp').
stack_pointer(13, 'sp').
frame_pointer(14, 'fp').
return_address(15, 'ret').

%% KEYWORDS

keyword(prog, 'PROGRAM').
keyword(beg, 'BEGIN').
keyword(end, 'END').

%% DYNAMIC RULES

:- dynamic get_set_device/2.
:- dynamic dev_on_program/2.
:- dynamic dev_off_program/2.
:- dynamic instr_count/1.
:- dynamic get_set_address/2.
:- dynamic id/1.
	
%% ASSEMBLER MAIN

assemble_file(InputFile, OutputFile) :-
    % Set initial instruction count to zero.
    asserta(instr_count(0)),

    % Read in input and tokenize
    open(InputFile, read, In, [eof_action(eof_code)]),
    see(In),
    tokenize_stream(In, Tokens), 
    write('tokenized: '), write(Tokens), nl,

    % Parse tokens and generate machine code
    open(OutputFile, write, Out),
    see(Out),
    program(Tokens, BinaryString),
    write('binary string constructed: '), nl, write(BinaryString), nl,
    write(Out, BinaryString),
    close(Out),
    close(In).
	
assemble_command(Command, BinaryString) :-
	% Set initial instruction count to zero.
    asserta(instr_count(0)),
	
	% Tokenize command
	tokenize_command(Command, CommandTokens), 
	% Parse command
	parse_command(CommandTokens), write('made it to the end, yay!').


%% COMMAND TOKENIZING/PARSING RULES

tokenize_command(Command, Return):-
	atomic_list_concat(Return, ' ', Command).

parse_command(['SET', Id]) :-
	% SET pseudo operation
	% Find a device register that isn't set
	findall(Reg1, dev_reg(_, Reg1), L1), 
	findall(Reg2, get_set_device(_, Reg2), L2),
	subtract(L1, L2, [DevReg|Other]),
	% Get character list for parsing identifier
	atom_chars(Id, Chars),
	% Test conditions for the ID, and if it passes the test set the device.
	identifier(Chars), \+keyword(_, Id),
	assert(get_set_device(Id, DevReg)).
	
parse_command(['CLEAR', Id]) :-
	write('made it to clear'), nl,
	% CLEAR pseudo operation
	get_set_device(Id, Reg),
	retract(get_set_device(Id, Reg)),
	retract(dev_on_program(Id, Reg)),
	retract(dev_off_program(Id, Reg)).

parse_command(['ON', Id]) :-
	% Generate assembly program to turn on a device
	dev_on_program(Id, ProgramTokens).

parse_command(['ON', Id]) :-
	% Generate assembly program to turn on a device
	get_set_device(Id, DevReg),
	ProgramTokens = ['PROGRAM', 'ON', 'g1', '1', 'g2', '2', 'BEGIN', 'SWITCHON', 'BEQ', 'g1', DevReg, 'g2', 'ADDI', DevReg, '1', 'NOOP', 'END', 'END'], 
	assert(dev_on_program(Id, ProgramTokens)).
	
parse_command(['OFF', Id]) :-
	% Generate assembly program to turn a device off
	dev_off_program(Id, ProgramTokens).
	
parse_command(['OFF', Id]) :-
	% Generate assembly program to turn a device off
	get_set_device(Id, DevReg),
	Program = ['PROGRAM', 'OFF', 'g1', '2', 'BEGIN', 'SWITCHOFF', 'BEQ', 'zero', DevReg, 'g1', 'ADDI', DevReg, '-1', 'NOOP', 'END', 'END'], 
	assert(dev_off_program(Id, ProgramTokens)).
	
%% STREAM TOKENIZING RULES

check_token(Stream, Token, [Token, Id]) :-
    keyword(_, Token),
	write('found keyword'), nl,
    \+at_end_of_stream(Stream),
    read_token(Stream, Id),
    atom_chars(Id, Chars),
    identifier(Chars),
    \+keyword(_,Chars),
    instr_count(Current),
    assert(get_set_address(Id, Current)).

check_token(_, Token, [Token]) :-
    instruction_set(OpCodes),
    member(Token, OpCodes),
	write('found instruction'), nl,
    inc_instr_count.

tokenize_stream(Command, []) :-
    at_end_of_stream(Stream),!.

tokenize_stream(Stream, Return) :-
    read_token(Stream, Token),
    write(Token), nl,
    check_token(Stream, Token, Checked) ->
    	tokenize_stream(Stream, Tail),
    	append(Checked, Tail, Return);
    tokenize_stream(Stream, Tail),
    append([Token], Tail, Return).

read_token(Stream, '') :-
    at_end_of_stream(Stream),!.

read_token(Stream, Return) :-
    get_char(Stream, Char),
    char_type(Char, alnum) ->
        E = '',
        atomic_concat(Char, E, NextReturn1),
        read_token(Stream, NextReturn2),
        atomic_concat(NextReturn1, NextReturn2, Return);
    char_type(Char, white) ->
        Return = ''.

%% GRAMMAR PARSING RULES

program([Head, Prog_id|Tail], BodyReturn) :-
	keyword(prog, Head),
    identifier(Prog_id),
    \+keyword(_,Prog_id),
	write('found valid program identifier'),
    body(Tail, BodyReturn).

body(['END'], '').

body([RegAtom, ValueAtom|Tail], Return) :-
    % Check if the next token is a register number; then check that the following token is a value.
    reg(_, RegAtom),
    atom_number(ValueAtom, Value),
    integer(Value),
    write('value: '), write(Value), nl,
    instruction(['ADDI', RegAtom, Value], InstrString),
    inc_instr_count,
    body(Tail, BodyReturn),
    string_concat(InstrString, BodyReturn, Return).

body([Head, Routine_id|Tail], RoutineReturn) :-
    % Check if the next token is the 'BEGIN' keyword; then check that the following token is an identifier.
    keyword(_, Head),
    identifier(Routine_id),
    \+keyword(_,Routine_id),
    instr_count(Current),
    assert(get_set_address(Routine_id, Current)),
    routine(Tail, RoutineReturn).

routine([HeadToken|TailTokens], Return) :-
    % Until you find the 'END' keyword, continue parsing instructions.
    \+keyword(end, HeadToken),
    instruction([HeadToken|TailTokens], InstructionReturn),
    inc_instr_count,
    routine(TailTokens, RoutineReturn),
    string_concat(InstructionReturn, RoutineReturn, Return).

instruction([Head, Src1, Src2, Dest1|_], Return) :-
    % Arithmetic Instruction Type
    mathop(Op, Head),
    to_binary_string(Op, OpString),
    reg(Num1, Src1), to_binary_string(Num1, Src1String), 
    reg(Num2, Src2), to_binary_string(Num2, Src2String),
    reg(Dest2, Dest1), to_binary_string(Dest2, DestString),
    string_concat(OpString, Src1String, Temp1),
    string_concat(Temp1, Src2String, Temp2),
    string_concat(Temp2, DestString, Return).

instruction([Head, Src, Imm|_], Return) :-
    write('instruction, head: '), write(Head), write(', src: '), write(Src), write(', imm: '), write(Imm), nl,
    % Immediate Instruction Type
    immop(Op, Head),
    write('op: '), write(Op), nl,
    to_binary_string(Op, OpString),
    reg(Num, Src), to_binary_string(Num, SrcString),
    write('reg: '), write(Num), nl,
    data_address(Imm, DataString),
    write('data string: '), write(DataString), nl,
    string_concat(OpString, SrcString, Temp),
    string_concat(Temp, DataString, Return).

instruction([Head, JumpAddr|_], Return) :-
    % Jump Instruction Type
    jumpop(Op, Head),
    to_binary_string(Op, OpString),
    instruction_address(JumpAddr, JumpAddrString),
    % Check whether the jump address corresponds to an already written rule
    get_set_address(_, JumpAddr),
    string_concat(OpString, JumpAddrString, Return).

inc_instr_count :-
    instr_count(Current),
    Inc is integer(Current) + 1,
    asserta(instr_count(Inc)).

identifier([]).

identifier([Head|Tail]) :- 
    char_type(Head, upper),
    identifier(Tail).

identifier(Atom) :-
    atom(Atom),
    atom_chars(Atom, AtomChars),
    identifier(AtomChars).

instruction_address(Num, Return) :-
    % Test that the integer is in the correct range and is, indeed, a integer.
    integer(Num) >= integer(0), integer(Num) =< integer(255),

    % Convert the base 10 integer to its binary (unsigned, 8-bit) representation, and 'return' it.
    to_binary_string(Num, Return),

    string_length(Return, Length),
    FillLen = 8 - integer(Length),
    sign_extend(0, FillLen, Result),
    atom_concat(Result, Return, Return).

data_address(Num, Return2) :-
    write('data address of num: '), write(Num), nl,
    % Test that the integer is in the correct range and is, indeed, a integer.
    Num >= 256, Num =< 4095,

    % Convert the base 10 integer to its binary (unsigned, 12-bit) representation, and 'return' it. 
    to_binary_string(Num, Return1),

    string_length(Return1, Length),
    FillLen = 12 - integer(Length),
    sign_extend(0, FillLen, Result),
    atom_concat(Result, Return1, Return2).

data_value(Num, Return) :-
    % Negative case

    % Test that the integer is in the correct range and is, indeed, a integer, then
    % convert the base 10 integer to its binary (signed, 16-bit two's complement) representation, and 'return' it.

    integer(Num) >= integer(-32767), integer(Num) < 0, 
    PosNum is integer(Num) * -1,
    Temp1 is \PosNum, 
    Temp2 is integer(Temp1) + 1,
    to_binary_string(Temp2, NextReturn), 
    write(NextReturn),

    % Sign extend the result

    string_length(NextReturn, Length),
    FillLen = 16 - integer(Length),
    sign_extend(1, FillLen, Result),
    atom_concat(Result, NextReturn, Return).

data_value(Num, Return) :-
    % Positive case

    % Test that the integer is in the correct range and is, indeed, a integer, then
    % convert the base 10 integer to its binary (signed, 16-bit two's complement) representation, and 'return' it.

    integer(Num) =< integer(32767), integer(Num) >= 0,
    to_binary_string(Num, NextReturn),

    % Sign extend the result

    string_length(NextReturn, Length),
    FillLen = 16 - integer(Length),
    sign_extend(0, FillLen, Result),
    atom_concat(Result, NextReturn, Return).

to_binary_string(0, '0').

to_binary_string(1, '1').

to_binary_string(-1, '1').

%to_binary_string(_, 'X').

to_binary_string(Num, Return) :-
     integer(Num) < 0,
     NewNum1 is integer(Num) * -1,
     max_bits(NewNum1, Max),
     NewNum2 is 2**integer(Max) - integer(NewNum1),
     NextNum is integer(NewNum2)//2,
     Remainder is integer(NewNum2) mod 2,
     to_binary_string(NextNum, NextReturn),
     atom_concat(NextReturn, Remainder, Return).

to_binary_string(Num, Return) :-
     NextNum is integer(Num) // 2,
     Remainder is integer(Num) mod 2,
     to_binary_string(NextNum, NextReturn),
     atom_concat(NextReturn, Remainder, Return).

sign_extend(FillChar, Bits, Return) :-
    integer(Bits) > 1,
    E = '',
    string_concat(FillChar, E, NextReturn1),
    sign_extend(FillChar, integer(Bits) - 1, NextReturn2),
    string_concat(NextReturn1, NextReturn2, Return).

sign_extend(FillChar, _, Return) :-
    E = '',
    string_concat(FillChar, E, Return).

max_bits(0, 1).

max_bits(Number, Sum) :-
    integer(Number) > 0, 
    Next is integer(Number)//2,
    max_bits(Next, R),
    Sum is integer(R) + 1.