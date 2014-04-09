% Define facts

%% OPCODES %%

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
	
%% PSEUDO OPERATIONS

pseudo('SET').
pseudo('CLEAR').
pseudo('ON').
pseudo('OFF').
pseudo('SWITCH').

%% REGISTERS %%

reg(0, 'r0').
reg(1, 'r1').
reg(2, 'r2').
reg(3, 'r3').
reg(4, 'r4').
reg(5, 'r5').
reg(6, 'r6').
reg(7, 'r7').
reg(8, 'r8').
reg(9, 'r9').
reg(10, 'r10').
reg(11, 'r11').
reg(12, 'r12').
reg(13, 'r13').
reg(14, 'r14').
reg(15, 'r15').

%% KEYWORDS

keyword(prog, 'PROGRAM').
keyword(block, 'BEGIN').
end('END').

%% DYNAMIC RULES

:- dynamic device_alias/2.
:- dynamic instr_count/1.
:- dynamic get_set_address/2.
:- dynamic id/1.

%% ASSEMBLER MAIN

assemble(InputFile, OutputFile) :-
	% Set initial instruction count to zero.
	asserta(instr_count(0)),
	
	% Read in input and tokenize
	open(InputFile, read, In, [eof_action(eof_code)]),
	see(In),
	tokenize(In, Tokens), 
	write('tokenized'),
	close(In),
	
	% May need to do a 'first pass' here to identify all routines and assign them addresses first
	
	% Parse tokens and generate machine code
	open(OutputFile, write, Out),
	see(Out),
	program(Tokens, BinaryString),
	write(Out, BinaryString),
	close(Out).
	
%% TOKENIZING RULES

check_token(Stream, Token, Return) :-
	write('checking token'), nl,
	instruction_set(OpCodes),
	keyword(_, Token) -> 
		\+at_end_of_stream,
		read_token(Stream, Id),
		atom_chars(Id, Chars),
		identifier(Chars),
		\+keyword(_,Chars),
		assert(get_set_address(Id, instr_count(Current))),
		Return = [Token, Id];
	
	member(Token, OpCodes) ->
		Return = Token,
		inc_instr_count.

tokenize(Stream, Return) :-
	at_end_of_stream,
	write('tokenize found eof'), nl,
	E = '',
	F = '',
	atomic_concat(E, F, Return).
	
tokenize(Stream, Return) :-
	\+at_end_of_stream,
	read_token(Stream, Token),
	write(Token), nl,
	%check_token(Stream, Token, Checked) ->
	%	tokenize(Stream, Tail),
	%	append(Checked, Tail, Return);
	tokenize(Stream, Tail),
	append(Token, Tail, Return).
		
read_token(Stream, Return) :-
	at_end_of_stream,
	write('read_token found eof'), nl, 
	E = '',
	F = '',
	atomic_concat(E, F, Return).
	
read_token(Stream, Return) :-
	\+at_end_of_stream,
	get_char(Stream, Char),
	char_type(Char, alnum) ->
		E = '',
		atomic_concat(Char, E, NextReturn1),
		read_token(Stream, NextReturn2),
		atomic_concat(NextReturn1, NextReturn2, Return);
	char_type(Char, white) ->
		E = '',
		F = '',
		atomic_concat(E, F, Return).
		
%% GRAMMAR PARSING RULES

program([Head|Tail], Return) :- 
	keyword(program, Head),
	[Prog_id|X] = Tail, 
	identifier(Prog_id),
	\+keyword(_,Prog_id),
	body(X, RemainingTokens),
	end(RemainingTokens).

body([], Return) :-
	Return = ''.
	
body([Head|Tail], Return) :-
	% Check if the next token is a register number; then check that the following token is a value.
	reg(X, Head) ->
		[Value|X] = Tail, 
		instruction(['ADDI', Head, Value], InstrString),
		inc_instr_count,
		body(X, BodyReturn),
		string_concat(InstrString, BodyReturn, Return);
	
	% Check if the next token is an identifier; then check that the following token is a value (constant).
	
	% Check if the next token is the 'BEGIN' keyword; then check that the following token is an identifier.
	keyword(routine, Head) ->
		[Routine_id|X] = Tail,
		identifier(Routine_id),
		\+keyword(_,Routine_id),
		assert(get_set_address(Routine_id, instr_count(Current))),
		routine(X, RoutineReturn),
		string_concat(RoutineReturn, '', Return).
	
routine(Tokens, Return) :-
	% Until you find the 'END' keyword, continue parsing instructions.
	\+end(Head),
	instruction(Tokens, InstructionReturn),
	inc_instr_count,
	routine(Tokens, RoutineReturn),
	string_concat(InstructionReturn, RoutineReturn, Return).
	
instruction([Head|Tail], Return) :-
	% Arithmetic Instruction Type
	mathop(Op, Head) ->
		to_binary_string(Op, OpString),
		[Src1, Src2, Dest1|RemainingTokens] = Tail,
		reg(Num1, Src1), to_binary_string(Num1, Src1String), 
		reg(Num2, Src2), to_binary_string(Num2, Src2String),
		reg(Dest2, Dest1), to_binary_string(Dest2, DestString),
		string_concat(Op, Src1String, Temp1),
		string_concat(Temp1, Src2String, Temp2),
		string_concat(Temp2, DestString, Return);
	
	% Immediate Instruction Type
	immop(Op, Head) ->
		to_binary_string(Op, OpString),
		[Src, Imm|RemainingTokens] = Tail,
		reg(Num, Src), to_binary_string(Num, SrcString),
		data_address(Imm, DataString),
		string_concat(Op, SrcString, Temp),
		string_concat(Temp, DataString, Return);
	
	% Jump Instruction Type
	jumpop(Op, Head) ->
		to_binary_string(Op, OpString),
		[JumpAddr|RemainingTokens] = Tail,
		instruction_address(JumpAddr, JumpAddrString),
		% Check whether the jump address corresponds to an already written rule
		get_set_address(Routine, JumpAddr),
		string_concat(Op, JumpAddrString, Return).
		
inc_instr_count :-
	instr_count(Current),
	Inc is integer(Current) + 1,
	asserta(instr_count(Inc)).
	
identifier([]).

identifier([Head|Tail]) :- 
	char_type(Head, upper),
	identifier(Tail).
	
instruction_address(Num, Return) :-
	% Test that the integer is in the correct range and is, indeed, a integer.
	integer(Num) >= integer(0), integer(Num) =< integer(255),
	
	% Convert the base 10 integer to its binary (unsigned, 8-bit) representation, and 'return' it.
	to_binary_string(Num, Return),
	
	string_length(Return, Length),
	FillLen = 8 - integer(Length),
	sign_extend(0, FillLen, Result),
	atom_concat(Result, Return, Return).

data_address(Num, Return) :-
	% Test that the integer is in the correct range and is, indeed, a integer.
	integer(Num) >= integer(256), integer(Num) =< integer(4095),
	
	% Convert the base 10 integer to its binary (unsigned, 12-bit) representation, and 'return' it. 
	to_binary_string(Num, Return),
	
	string_length(Return, Length),
	FillLen = 12 - integer(Length),
	sign_extend(0, FillLen, Result),
	atom_concat(Result, Return, Return).
	
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

to_binary_string(Num, Return) :-
	integer(Num) < 0 ->
		NewNum1 is integer(Num) * -1,
		max_bits(NewNum1, Max),
		NewNum2 is 2**integer(Max) - integer(NewNum1),
		NextNum is integer(NewNum2)//2,
		Remainder is integer(NewNum2) mod 2,
		to_binary_string(NextNum, NextReturn),
		atom_concat(NextReturn, Remainder, Return);
	
	NextNum is integer(Num)// 2,
	Remainder is integer(Num) mod 2,
	to_binary_string(NextNum, NextReturn),
	atom_concat(NextReturn, Remainder, Return).
	
sign_extend(FillChar, Bits, Return) :-
	integer(Bits) > 1 ->
		E = '',
		string_concat(FillChar, E, NextReturn1),
		sign_extend(FillChar, integer(Bits) - 1, NextReturn2),
		string_concat(NextReturn1, NextReturn2, Return);
	E = '',
	string_concat(FillChar, E, Return).

max_bits(0, 1).

max_bits(Number, Sum) :-
	integer(Number) > 0, 
	Next is integer(Number)//2,
	max_bits(Next, R),
	Sum is integer(R) + 1.
	


	

	
	
	
	
	
	
	
	
	
	
	
	
	





	
