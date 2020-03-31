:-  use_module(library(tokenize)).
%:- use_module(library(make)).
%make:reload_file(pl).
 
/* 
 %{write(Token)}
 %format('~w~n', [X])

test --> ws, a.

%test([],[]).
a --> [a].
*/
run(Ast) :- string_tokens(" program factorial; begin read value; count := 1; result := 1; while count < value do begin count := count + 1; result := result * count end; write result end",Tokens),
compile(Tokens,Ast).%, write(Ast).% format('~w~n',Ast).

string_tokens(String, Tokens) :-
  string_chars(String, Chars),
  phrase(tokens(Tokens), Chars). 

% Definite Clause Grammar (DCG)

tokens([Token|Tokens]) --> ws, token(Token), ws, !, tokens(Tokens).%variable, charlist,emptylist
tokens([])             --> [].



ws --> [W], { char_type(W, space) }, ws.%charlist,variable
ws --> [].

token(P) --> [C], { char_type(C, punct), \+char_type(C, quote), string_chars(P, [C]) }.%variable, charlist,variable
token(Q) --> quote(Cs), {   string_chars(Q, Cs) }.
token(W) --> word(Cs), {   string_chars(W, Cs)  }.

quote([Quote|Ls])  --> [Quote], { char_type(Quote, quote)  }, quote_rest(Quote, Ls).%variable, charlist,variable
quote_rest(Quote, [L|Ls]) --> [L], { L \= Quote }, quote_rest(Quote, Ls).
quote_rest(Quote, [Quote]) --> [Quote]. 


word([L|Ls])      --> [L], { char_type(L, alnum) }, word_rest(Ls).%variable, charlist,variable
word_rest([L|Ls]) --> [L], { char_type(L, alnum) }, word_rest(Ls).
word_rest([])     --> [].
/*
 
quote(Quote)  --> [Quote], { char_type(Quote, quote)  }.
*/

compile(Tokens, ObjectCode) :-
  parse(Tokens, ObjectCode).
  %,
  %encode(Structure, Dictionary, Code),
 % assemble(Code, Dictionary, ObjectCode), !.
  
  

%% parse(+Tokens, -Ast) is det.
% The predicate parse is just an interface to the DCG, whose top-level predicate is pl_program.
% ~~~
% Tokens = ["program", "test1", ";", 
%             "begin", 
%               "write", "x", "+", "y", "-", "z", "/", "2", 
%             "end"],
% parse(Tokens, Ast),
% Ast =  (pl_write	(expr("+", 
% 								name(x), expr("-", 
% 													name(y), expr("/", 
% 																		name(z), number(2)))
% 																							))	
% 
% 
% 																								;void).
%  
% ~~~
/*"
  program factorial;
    begin
      read value;
      count := 1;
      result := 1;
      while count < value do
        begin
          count := count + 1;
          result := result * count
        end;
      write result
    end"
	
	*/
%	(pl_read(value);assign(count, number(1));assign(result, number(1));while(compare("<", name(count), name(value)),  (assign(count, expr("+", name(count), number(1)));assign(result, expr("*", name(result), name(count)));void));pl_write(name(result));void) ;
parse(Tokens, Ast) :-
  phrase(pl_program(Ast), Tokens).

%% pl_program(-S) is det
% The first statement of any PL program must be a program statement.

pl_program(S)              --> ["program"], identifier(_Name), [";"], statement(S).

statement((S;Ss))          --> ["begin"], statement(S), rest_statements(Ss).
statement(assign(X,E))     --> identifier(X), [":", "="], expression(E).
statement(if(T,S1,S2))     --> ["if"], test(T), ["then"], statement(S1), ["else"], statement(S2).
statement(while(T,S))      --> ["while"], test(T), ["do"], statement(S).
statement(pl_read(X))      --> ["read"], identifier(X).
statement(pl_write(X))     --> ["write"], expression(X).
rest_statements((S;Ss))    --> [";"], statement(S), rest_statements(Ss).
rest_statements(void)      --> ["end"].

expression(X)              --> pl_constant(X).
expression(expr(Op, X, Y)) --> pl_constant(X), arithmetic_op(Op),    expression(Y).

arithmetic_op("+")         --> ["+"].
arithmetic_op("-")         --> ["-"].
arithmetic_op("*")         --> ["*"].
arithmetic_op("/")         --> ["/"].

pl_constant(number(X))     --> pl_integer(X), !. % Moved up with cut to avoid numbers appearing as name('1')
pl_constant(name(X))       --> identifier(X).

pl_integer(X)              --> [Y], { number_string(X, Y) }.
identifier(X)              --> [Y], { atom_string(X, Y) }.

test(compare(Op, X, Y))    --> expression(X), comparison_op(Op), expression(Y).

comparison_op("=")         --> ["="].
comparison_op("!=")        --> ["!","="].
comparison_op(">")         --> [">"].
comparison_op("<")         --> ["<"].
comparison_op(">=")        --> [">","="].
comparison_op("<=")        --> ["<","="].

% Code Generator

%% encode(+Structure,-Dictionary,-RelocatableCode)
% RelocatableCode is generated from the parsed Structure (Ast)
% building a Dictionary associating variables with addresses.
% An incomplete ordered binary tree is used to implement it, as described in Section 15.3. The predicate
% lookup(Name,D,Value) (Program 15.9) is used for accessing the incomplete binary tree.
% The functor ; is used to denote sequencing.

encode((X;Xs), D,(Y;Ys)) :-
  encode(X, D, Y),
  encode(Xs, D, Ys).

encode(void, _D, no_op).

encode(assign(Name, E), D, (Code; instr(store, Address))) :-
  lookup(Name, D, Address),
  encode_expression(E, D, Code).

encode(if(Test, Then, Else), D, (TestCode; ThenCode; instr(jump, L2); label(L1); ElseCode; label(L2))) :-
  encode_test(Test, L1, D, TestCode),
  encode(Then, D, ThenCode),
  encode(Else, D, ElseCode).

encode(while(Test, Do), D, (label(L1); TestCode; DoCode; instr(jump, L1); label(L2))) :-
  encode_test(Test, L2, D, TestCode),
  encode(Do, D, DoCode).

encode(pl_read(X), D, instr(read, Address)) :-
  lookup(X, D, Address).

encode(pl_write(E), D, (Code; instr(write, 0))) :-
  encode_expression(E, D, Code).

%% encode_expression(Expression, Dictionary, Code)
%    Code corresponts to an arithmetic Expression.

encode_expression(number(C), _D, instr(loadc, C)).

encode_expression(name(X), D, instr(load, Address)) :-
  lookup(X, D, Address).

encode_expression(expr(Op, E1, E2), D, (Load; Instruction)) :-
  single_instruction(Op, E2, D, Instruction),
  encode_expression(E1, D, Load).
  
encode_expression(expr(Op, E1, E2), D, Code) :-
  \+single_instruction(Op, E2, D, _Instruction),
  single_operation(Op, E1, D, E2Code, Code),
  encode_expression(E2, D, E2Code).

single_instruction(Op, number(C), _D, instr(OpCode, C)) :-
  literal_operation(Op, OpCode).

single_instruction(Op, name(X), D, instr(OpCode, A)) :-
  memory_operation(Op, OpCode), lookup(X, D, A).

single_operation(Op, E, D, Code, (Code; Instruction)) :-
  commutative(Op),
  single_instruction(Op, E, D, Instruction).

single_operation(Op, E, D, Code, (Code; instr(store, Address); Load; instr(OpCode, Address))) :-
  \+commutative(Op),
  lookup('$temp', D, Address),
  encode_expression(E, D, Load),
  op_code(E, Op, OpCode).

op_code(number(_C), Op, OpCode) :-
  literal_operation(Op, OpCode).
  
op_code(name(_C), Op, OpCode) :-
  memory_operation(Op, OpCode).

literal_operation("+", addc).
literal_operation("-", subc).
literal_operation("*", mulc).
literal_operation("/", divc).

memory_operation("+", add).
memory_operation("-", sub).
memory_operation("*", mul).
memory_operation("/", div).

commutative("+").
commutative("*").

encode_test(compare(Op, E1, E2), Label, D, (Code; instr(OpCode, Label))) :-
  comparison_opcode(Op, OpCode),
  encode_expression(expr("-", E1, E2), D, Code).

comparison_opcode("=", jumpeq).
comparison_opcode("!=", jumpne).
comparison_opcode(">", jumpgt).
comparison_opcode("<", jumplt).
comparison_opcode(">=", jumpge).
comparison_opcode("<=", jumple).

% The Assembler

%% assemble(+Code, +Dictionary, TidyCode) is det
% TidyCode is the result of assembling Code removing no_ops and labels and filling in the Dictionary

assemble(Code, Dictionary, TidyCode) :-
  tidy_and_count(Code, 1, N, TidyCode-(instr(halt,0);block(L))), % can't use \ as difference list delimeter
  N1 is N + 1,
  allocate(Dictionary, N1, N2),
  L is N2 - N1, !.

tidy_and_count((Code1;Code2), M, N, TCode1-TCode2) :-
  tidy_and_count(Code1, M, M1, TCode1-Rest),
  tidy_and_count(Code2, M1, N, Rest-TCode2).

tidy_and_count(instr(X, Y), N, N1, (instr(X,Y);Code)-Code) :-
  N1 is N + 1.

tidy_and_count(label(N), N, N, Code-Code).

tidy_and_count(no_op, N, N, Code-Code).

%% lookup(Key, Dictionary, Value)
% modified to use SWI-Prolog's "Standard Order of Terms" comparison to handle variables
% http://www.swi-prolog.org/pldoc/man?section=bidicts
lookup(Key, dict(Key, X, _Left, _Right), Value) :-
  !, X = Value.
lookup(Key, dict(Key1, _X, Left, _Right), Value) :-
  Key @< Key1,
  lookup(Key, Left, Value).
lookup(Key, dict(Key1, _X, _Left, Right), Value) :-
  Key @> Key1,
  lookup(Key, Right, Value).

allocate(void, N, N).
allocate(dict(_Name, N1, Before, After), N0, N) :-
  allocate(Before, N0, N1),
  N2 is N1 + 1,
  allocate(After, N2, N).
