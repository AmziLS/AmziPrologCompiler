% -*-Prolog-*-  
% assemb indented on 12/23/2001 by 'JOLI' 1.0.

%---------------------------------------------------------------------
%
% assemb.pro -- assembler from WAL code --> PCode
%
% Copyright (c) 1992-2002 Amzi! inc. All Rights Reserved.
%
% $Log: assemb.pro,v $
% Revision 1.2  2005/12/06 23:24:47  dennis
% writeing of '_' fixed
%
% Revision 1.1.1.1  2003/09/11 02:15:10  dennis
% Starting release 7.0
%
% Revision 1.14  2003/08/23 03:27:31  dennis
% cut working for debug64 compiled code
%
% Revision 1.13  2002/05/15 16:59:06  dennis
% Final fixes for last 6.1 build, 80
%
% Revision 1.12  2002/04/25 03:42:22  dennis
% more documentation, logicbase.htm, and some fiddling with sources
%
% Revision 1.11  2002/04/02 22:52:43  dennis
% Moved the hotel two feet to the right, changing arity and xi
% in .plm files to be 2 bytes rather than 1.
%
% Revision 1.10  2002/02/13 03:19:59  dennis
% reals changed to have a LReal class, moved to file of same name,
% math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
% to reflect various options for numbers, lexcept modified so anyone, even
% non-engine objects, can throw LExcept objects.
%
% Revision 1.9  2002/01/28 06:29:18  dennis
% changes for parsing numbers, handling different options
%
% Revision 1.8  2002/01/20 20:48:05  ray
% revised real divide, printReal
%
% Revision 1.7 2001/10/19 01:37:59 dennis
% compiler bugs, still not found, but noted that X registers
% are really restricted to 255 because of flewrite in assemb.pro,
% should change some day.
%
% Revision 1.6 2001/04/16 05:21:13 dennis
% hacked together some fixes for sio/lex to be better friends,
% merged other changes, added new samples
%
% Revision 1.5 2001/04/01 18:05:59 ray
% Removed 'flow' statements
%
% Revision 1.4 2001/04/01 15:54:51 ray
% Modified compiler and loader for fixed data.
%
% Revision 1.3 2001/02/08 22:56:45 dennis
% string bug fixes, modularized compiler and listener
%
% Revision 1.2 2001/01/30 16:47:27 dennis
% Made, after many trials, alib into amzi_system module.
%
% Revision 1.1.1.1 2000/12/29 02:18:05 dennis
% moved to a6
%
% Revision 1.13 2000/11/30 15:47:27 ray
% made arg, nth and prime bilateral.
% made real length and exponent 12 bits for maximum range
%
% Revision 1.12 2000/11/10 17:09:18 ray
% Restricted real length to 255 in order to add max length, for protection
%
% Revision 1.11 2000/10/30 15:11:45 ray
% Extended compiler, loader and linker for real data.
% Added arithmetic primitives for rational data in alib.pro
%
% Revision 1.10 2000/10/20 12:40:51 ray
% Moved =:= out of alib to accomodate reals.
% Various repairs and enhancements
%
% Revision 1.9 2000/10/07 17:47:19 ray
% added q_cf and fourierPrime
%
% Revision 1.8 2000/09/25 02:11:18 dennis
% first version of modules working, runs the modular version of
% duck world. still needs import and export. release 6.1.1
%
% Revision 1.7 2000/09/15 21:42:24 dennis
% 12->13
%
% Revision 1.6 2000/08/21 21:48:28 ray
% *** empty log message ***
%
% Revision 1.5 2000/03/28 23:47:48 dennis
% Changed all tabs to three spaces, and also changed Logic Server
% to use void* for TERM externally and cast to Cell* in LEngine
% implementation.
%
% Revision 1.4 2000/03/28 01:05:14 dennis
% merged Ray's changes with bigdig. bigdig is at point where
% new Cell class is used, but there are no modules in the system.
%
% Revision 1.3.2.2 2000/02/28 23:57:54 dennis
% removed local atoms from engine
%
% Revision 1.3.2.1 2000/02/26 20:56:12 dennis
% Removed local atoms from compiler, and old module support, so
% compiler and listener are all global for now. Also made member/2
% and friends built-ins as well as the bug predicates.
%
%
%/04/08/99 Ray recoded find_atom for clarity
%---------------------------------------------------------------------

:- body(amzi_compiler).

/* Original Alan comments

    Xi registers get mapped to &X[i] in pld_code loader, so we have to be
    careful to:

    encode index as byte(I, ArgSkel) in code stream if
       ArgSkel = x(_) or ArgSkel = y(_) or as
       xindex(I) if it is known to be an Xi
    
   Version 1.1 -- added module support
   -----------------------------------

   Hidden atoms
   Atom indices numbered from 1 and not 0 if is a module
   See pload.c for a complete rundown on how this hangs
       together

   Op codes now expand to one 16 bit word (the threaded address)
   so we have to add one to computed VPs

   Version 1.2 -- linked file support
   ----------------------------------

   Clauses are compiled individually, assembled individually
   and linked at the end. The new format for EACH CLAUSE is

   [Atom-Table-length]
   [--atom table--]
   [CodeLength]      % Length from next byte to end of code 
   [ClauseType]      % 1 if last clause in procedure else 0 
   [Functor]     
   [Arity]
   [Code]

   Version 3.0 -- target independent code
   --------------------------------------

   Offsets in generated code are no longer used to compute code
   lengths or address labels and offsets.

   Instead, label numbers are kept throughout the code that are
   resolved at load time.  The length of the code is also determined
   at load time.

   Since we have reduced the number of atoms liable to be written
   out at any given time (since it is one table per clause rather than
   one per procedure) we can now keep the atom table on the heap. So
   the element writing portions have two arguments - an input
   table and an output table.

   We should investigate whether a difference structure is a better idea.
*/

%-------------------------------------------------------------------
% Compiled Code header format:
%
% 00 - ff    indicates Prolog compiled code - used by consult
% 01 - 13    &x03 indicates .plm, &x10 indicates unicode
% 02 - 3d    version, 61 or 6.1 in this example
% 03 - 47    build, 71 in this example
% 04 - 00    checksum - not set here (used in registration)
% 05 - 00    product id - not set here (used in registration)
%
%-------------------------------------------------------------------

% If this is first clause then leave room for potential
% switch on term - try_me_else (where NTV is arity)
% Otherwise leave room for a retry_me_else
% The label of sot is -3 -- easily recognized 
% by the loader as bogus

assemble(FileHandle, Code, F/A, FirstClause, PhysicalP) :-
  (                       % push code onto Code, assemble and return PhysicalP
     FirstClause == yes ->
     Code1 = [switch_on_term(-1, -1, -1), try(A, else, 0)|Code] ;

     Code1 = [retry(else, 0)|Code]
  ),
  assemble1(Code1, F/A, BlockCode), % make obj code with var absolute branches
  allocate(FileHandle, BlockCode, PhysicalP1), % PhysicalP1 is code start addr
  PhysicalP is PhysicalP1 + 4, !.             % add the functor/arity words

assemble1(Code, Proc/Ar, block(atoms([Table]), [raw(Index), raw(Ar)|B])) :-
  Index = 0,
  block(Code, [[Proc, Index]], Table, B, []), !.

block([], Table, Table, B, B).
block([E|Rest], InTable, OutTable, Code, Link) :- % walk code, bldng atom table
  do_elem(E, InTable, OutSF, Code, L),
  block(Rest, OutSF, OutTable, L, Link).

find_atom(InTable, InTable, N, Index) :-      % look for atom index
  member([N, Index], InTable), !.
find_atom(InTable, OutTable, N, Index) :-     % not there, so cons it on
  InTable = [[_, OldI]|_],
  arith_plus(Index, OldI, 1),                 % put new Alist back
  OutTable = [[N, Index]|InTable].

% This is only called for unify_unsafe, and if its a yreg, then
% it needs to have the 0x8000 flag set so the engine can note
% the situation.

ri_reg(Ri, ByteCode) :-     % Map Ri notation to correct register offset value
  Ri =.. [XY, A],
  (
     A > 0x0fff ->
     throw(comperr($\nClause has more than 4095 variables$)) ;

     true
  ),
  (XY = y, Flag = 0x8000 ;  Flag = 0),
  ByteCode is (A - 1) \/ Flag, !.   % \/ is or


/*  
ri_reg(Ri, ByteCode) :-     % Map Ri notation to correct register offset value
  Ri =.. [XY, A],
  (
     A > 127 ->
     throw(comperr($\nClause has more than 127 variables$)) ;

     true
  ),
  (XY = y, Flag = 128 ;  Flag = 0),
  ByteCode is (A - 1) \/ Flag, !.
*/

/*
Get OpCodes

Note: WAL registers are numbered 1 - 8.  Our registers are
numbered 0 - 7 ... sigh. So for the moment let the assembler
relocate all register references by 1.
*/

% do_elem produces block code for one assmbly code element

do_elem(get(V, Arg1, x(A2)), AL, AL, [o(OP), xy(X1, Arg1), xindex(X2)|L], 
       L) :-
  (V = variable, OP = get_var(Arg1) ;  V = value, OP = get_val(Arg1)), !,
  Arg1 =.. [Reg, I],
  arith_plus(X1, I, -1),
  arith_plus(X2, A2, -1), !.
do_elem(get(constant, C, x(A2)), AL, NewAL, Code, L) :-
  (
     atom(C),
     find_atom(AL, NewAL, C, Val),
     Type = atom ;

     (string(C), Val = C, Type = string, NewAL = AL) ;

     %(short(C), Val = C, Type = int, NewAL = AL) ;

     %(long(C), Val = C, Type = long, NewAL = AL) ;
     (integer(C), Val = C, Type = integer, NewAL = AL) ;

     (fixed_real(C), Val = C, Type = fixed_real, NewAL = AL) ;

     %(float(C), Val = C, Type = float, NewAL = AL) ;
     (single_float(C), Val = C, Type = single_float, NewAL = AL) ;
     (double_float(C), Val = C, Type = double_float, NewAL = AL) ;

     (long_real(C), Val = C, Type = long_real, NewAL = AL)
  ), !,
  arith_plus(X2, A2, -1),
  Code = [op(1), cell(Type, Val), xindex(X2)|L], !.
do_elem(get(structure, F/A, x(I)), AL, NewAL, Code, L) :-
  find_atom(AL, NewAL, F, If),
  arith_plus(Xi, I, -1),
  Code = [op(3), atom(If), arity(A), xindex(Xi)|L], !.
do_elem(get_list(x(I)), AL, AL, [op(4), xindex(Xi)|L], L) :-
  arith_plus(Xi, I, -1), !.
do_elem(get_nil(x(I)), AL, AL, [op(2), xindex(Xi)|L], L) :-
  arith_plus(Xi, I, -1), !.
do_elem(put(V, Arg1, x(A2)), AL, AL, [o(OP), xy(X1, Arg1), xindex(X2)|L], 
       L) :-
  ((V = variable, OP = put_var(Arg1)) ;  (V = value, OP = put_val(Arg1))), !,
  Arg1 =.. [Reg, I],
  arith_plus(X1, I, -1),
  arith_plus(X2, A2, -1), !.
do_elem(put(unsafe_value, y(A1), x(A2)), AL, AL, Code, L) :-
  arith_plus(X1, A1, -1),
  arith_plus(X2, A2, -1),
  Code = [op(5), yindex(X1), xindex(X2)|L], !.
do_elem(put(constant, C, x(I)), AL, NewAL, [op(6), Cons, xindex(Xi)|L], L) :-
  (
     (atom(C), find_atom(AL, NewAL, C, Ia), Cons = cell(atom, Ia)) ;

     (string(C), Cons = cell(string, C), NewAL = AL) ;

     %(short(C), Cons = cell(int, C), NewAL = AL) ;

     (integer(C), Cons = cell(integer, C), NewAL = AL) ;

     (fixed_real(C), Cons = cell(fixed_real, C), NewAL = AL) ;

     (single_float(C), Cons = cell(single_float, C), NewAL = AL) ;
     (double_float(C), Cons = cell(double_float, C), NewAL = AL) ;

     (long_real(C), Cons = cell(long_real, C), NewAL = AL)
  ), !,
  arith_plus(Xi, I, -1), !.
do_elem(put_nil(x(A)), AL, AL, [op(7), xindex(X)|L], L) :-
  arith_plus(X, A, -1), !.
do_elem(put(structure, F/A, x(I)), AL, NewAL, Code, L) :-
  find_atom(AL, NewAL, F, If),
  arith_plus(Xi, I, -1),
  Code = [op(8), atom(If), arity(A), xindex(Xi)|L], !.
do_elem(put_list(x(I)), AL, AL, [op(9), xindex(Xi)|L], L) :-
  arith_plus(Xi, I, -1), !.

% unify opcodes
do_elem(unify(unsafe_value, Arg), AL, AL, [op(38), uindex(Xi)|L], L) :-
  ri_reg(Arg, Xi), !.
do_elem(unify(V, Arg), AL, AL, [o(CV), xy(Xi, Arg)|L], L) :-
  ((V = value, CV = unify_val(Arg)) ;  (V = variable, CV = unify_var(Arg))),
  Arg =.. [Reg, I],
  arith_plus(Xi, I, -1), !.
do_elem(unify(void, N), AL, AL, [op(25), int(N)|L], L) :- !.
do_elem(unify(constant, C), AL, NewAL, [op(24), CELL|L], L) :-
  (
     (atom(C), find_atom(AL, NewAL, C, Val), CELL = cell(atom, Val)) ;

     (string(C), CELL = cell(string, C), NewAL = AL) ;

     (integer(C), CELL = cell(integer, C), NewAL = AL) ;

     %(long(C), CELL = cell(long, C), NewAL = AL) ;

     (fixed_real(C), CELL = cell(fixed_real, C), NewAL = AL) ;

     (single_float(C), CELL = cell(single_float, C), NewAL = AL) ;
     (double_float(C), CELL = cell(double_float, C), NewAL = AL) ;

     (long_real(C), CELL = cell(long_real, C), NewAL = AL)
  ), !.

% Sequence Opcodes
do_elem(allocate(N), AL, AL, [op(14), int(N)|L], L) :- !.

do_elem(mod_call(Mod, Name/Arity, N), AL, NewAL, Code, L) :-
  find_atom(AL, AL2, Name, Ia),
  find_atom(AL2, NewAL, Mod, Imod),
  % OpCode = 55,                                % Omod_call
  Code = [op(55), atom(Imod), atom(Ia), arity(Arity), int(N)|L], !.
do_elem(mod_execute(Mod, Name/Arity), AL, NewAL, Code, L) :-
  find_atom(AL, AL2, Name, Ia),
  find_atom(AL2, NewAL, Mod, Imod),
  % OpCode = 56,                                % Omod_exec
  Code = [op(56), atom(Imod), atom(Ia), arity(Arity)|L], !.
do_elem(call(Name/Arity, N), AL, NewAL, Code, L) :-
  find_atom(AL, NewAL, Name, Ia),
  %OpCode = 10,
  Code = [op(10), atom(Ia), atom(Ia), arity(Arity), int(N)|L], !.
do_elem(execute(Name/Arity), AL, NewAL, Code, L) :-
  find_atom(AL, NewAL, Name, Ia),
  %OpCode = 12,
  Code = [op(12), atom(Ia), atom(Ia), arity(Arity)|L], !.
do_elem((Name/Arity), AL, NewAL, Code, L) :-
  find_atom(AL, NewAL, Name, Ia),
  Code = [op(13), atom(Ia), arity(Arity)|L], !.          %escape, C predicate
do_elem(OpCode, AL, AL, [op(OC)|L], L) :-
  atom(OpCode),
  obj_code(OpCode, OC), !.

/* 
labels bind to the current label number and are kept in code for loader.
note that these labels were earlier bound to other variables, so 
setting the value here sets it there as well due to the magic of Prolog  
*/
do_elem(label(Lbl), AL, AL, [op(37), label(Lbl)|L], L) :-
  cntr_dec(3, Lbl), !.

/* branch instructions */

/* [op_code, L1, L2, L3]  */
do_elem(switch_on_term(A, B, C), AL, AL,
       [s(switch_on_term, Ax, Bx, Cx)|L], 
       L) :-
  compute_lab(A, Ax),
  compute_lab(B, Bx),
  compute_lab(C, Cx), !.
do_elem(trust(else, fail), AL, AL, Code, L) :-
  Code = [op(22)|L], !.

% [op_code, L] 
do_elem(try(NTV, else, A), AL, AL, Code, L) :-
  (A == fail, Ax = 0 ;  Ax = A),
  Code = [s(try_me_else, Ax, NTV)|L], !.
do_elem(tryor(NTV, else, A), AL, AL, Code, L) :-
  (A == fail, Ax = 0 ;  Ax = A),
  Code = [s(try_me_or_else, Ax, NTV)|L], !.
do_elem(retry(else, A), AL, AL, Code, L) :-
  (A == fail, Ax = 0 ;  Ax = A),
  Code = [s(retry_me_else, Ax)|L], !.
do_elem(G, AL, AL, [s(X, A)|L], L) :-
  (G = retry(A) ;  G = trust(A) ;  G = goto(A)), !,
  functor(G, X, _), !.
do_elem(try(NTV, A), AL, AL, [s(try, A, NTV)|L], L) :- !.
do_elem(cutd(A), AL, AL, Code, L) :-
  Code = [s(cutd, A)|L], !.
do_elem(switch(constant, Size, Fudge), AL, AL, 
       [o(switch_on_constant), int(Size)|L], L) :-
  integer(Size), !.
do_elem(switch(structure, Size, Fudge), AL, AL, 
       [o(switch_on_structure), int(Size)|L], L) :-
  integer(Size), !.

/* the pair() pseusdo op - for now we build a literal table -- no fudging */

% [Name, Arity, Label ] 
do_elem(pair(Name/Arity, Label), AL, NewAL, 
       [s(pair, IName, Arity, Label)|L], L) :-
  integer(Arity),
  find_atom(AL, NewAL, Name, IName), !.

% CARE ! here the offset is from the CELL in the table 
do_elem(pair(C, Label), AL, NewAL, [s(pair, CELL, Label)|L], L) :-
  (
     (atom(C), find_atom(AL, NewAL, C, Val), CELL = cell(atom, Val)) ;

     (string(C), CELL = cell(string, C), NewAL = AL) ;

     %(short(C), CELL = cell(int, C), NewAL = AL) ;

     (integer(C), CELL = cell(integer, C), NewAL = AL) ;

     (fixed_real(C), CELL = cell(fixed_real, C), NewAL = AL) ;

     (single_float(C), CELL = cell(single_float, C), NewAL = AL) ;
     (double_float(C), CELL = cell(double_float, C), NewAL = AL) ;

     (long_real(C), CELL = cell(long_real, C), NewAL = AL)
  ), !.
%do_elem(null_byte, AL, NewAL, [byte(0)|L], L) :- !.
do_elem(null_byte, AL, NewAL, [byte(0)|L], L) :- 
  throw(comperr(`\nNull byte error`)).
do_elem(OpCode, AL, AL, L, L) :-
  throw(comperr($\nCannot assemble opcode $, OpCode)).

% report($\nCannot assemble opcode $), report(OpCode), report($\n\n$),
% !, halt.
/* 
Note - all of the fwrite's have been changed to flewrite, an undocumented
predicate that is the same as fwrite except it always writes in little
endian format.  This ensures compatibility of .plm files across environments. 
*/

allocate(File, block(atoms([Table]), Code), PhysicalP) :-
  fseek(File, 0, 1, Origin),      % reserve next int for length of atom-table 
  flewrite(File, 0, 1),
  write_atoms(Table, File, EndOfTable),

                        % fill in atom-table length, then repos to write code 
  fseek(File, Origin, 0, _),
  AtomTableSize is integer((EndOfTable - Origin) - 2),
  (AtomTableSize > 0x0000ffff -> throw(comperr(atomstoolong)) ;  true),
  flewrite(File, AtomTableSize, 1),
  fseek(File, EndOfTable, 0, _),              % now write code 
  flewrite(File, 0, 1),                       % reserve word for code length
  flewrite(File, 0, 1),                       % reserve int for clause type 
  fseek(File, 0, 1, PhysicalP),               % get phys addr of functor int 
  write_code(File, Code, EOFPos),             % write out the code 
  fseek(File, EndOfTable, 0, _),              % get back to write code length
  CodeLength is integer(EOFPos - EndOfTable - 2),
  (CodeLength > 0x0000ffff -> throw(comperr(codetoolong)) ;  true),
  flewrite(File, CodeLength, 1),
  fseek(File, EOFPos, 0, _).          % repos to End OF Code for next allocate

write_atoms([[A, I]|T], File, _) :-           % write atoms in reverse order
  do_one(A, I, T, File),
  fail.
write_atoms(_, File, FPos) :-
  fseek(File, 0, 1, FPos).

do_one(A, I, T, File) :-
  write_atoms(T, File, _),
  write_atom(File, A),
  (is_unicode -> flewrite(File, 0, 1) ;  flewrite(File, 0, 0)), !. % the null

write_atom(File, A) :-
  is_unicode,
  atom_codes(A, Codes),
  (current_prolog_flag(vba, on) ->
     Codes2 = [8|Codes]
     ;
     Codes2 = Codes ),
  member(C, Codes2),
  flewrite(File, C, 1),
  fail.
write_atom(File, A) :-
  not(is_unicode),
  atom_codes(A, Codes),
  (current_prolog_flag(vba, on) ->
     Codes2 = [8|Codes]
     ;
     Codes2 = Codes ),
  member(C, Codes2),
  flewrite(File, C, 0),
  fail.
write_atom(_, _).

write_code(File, Code, Pos) :-
  member(H, Code),
  w_code_seg(H, File),
  fail.
write_code(File, _, Pos) :-
  fseek(File, 0, 1, Pos).

%w_code_seg(I, File) :-
%  integer(I),
%  flewrite(File, I, 0), !.
w_code_seg(s(try_me_else, Branch, NTV), File) :-
  w_code_seg(o(try_me_else), File),
  w_code_seg(int(Branch), File),
  w_code_seg(int(NTV), File), !.
w_code_seg(s(try_me_or_else, Branch, NTV), File) :-
  w_code_seg(o(try_me_or_else), File),
  w_code_seg(int(Branch), File),
  w_code_seg(int(NTV), File), !.
w_code_seg(s(retry_me_else, Branch), File) :-
  w_code_seg(o(retry_me_else), File),
  w_code_seg(int(Branch), File), !.
w_code_seg(s(try, Branch, NTV), File) :-
  w_code_seg(o(try), File),
  w_code_seg(int(Branch), File),
  w_code_seg(int(NTV), File), !.
w_code_seg(s(B, Branch), File) :-
  (B == retry ;  B == trust ;  B == goto),
  w_code_seg(o(B), File),
  w_code_seg(int(Branch), File), !.
w_code_seg(s(cutd, Branch), File) :-
  w_code_seg(o(cutd), File),
  w_code_seg(int(Branch), File), !.
w_code_seg(s(switch_on_term, A1, A2, A3), File) :-
  w_code_seg(o(switch_on_term), File),
  w_code_seg(int(A1), File),
  w_code_seg(int(A2), File),
  w_code_seg(int(A3), File), !.
w_code_seg(s(pair, Name, Arity, A), File) :-
  w_code_seg(atom(Name), File),
%  w_code_seg(byte(Arity), File),
  w_code_seg(arity(Arity), File),
  w_code_seg(int(A), File), !.
w_code_seg(s(pair, CELL, A), File) :-
  w_code_seg(CELL, File),
  w_code_seg(int(A), File), !.
w_code_seg(o(Code), File) :-
  obj_code(Code, OC),
  flewrite(File, OC, 0), !.
w_code_seg(op(OC), File) :-
  flewrite(File, OC, 0), !.
/*
w_code_seg(byte(B), File) :-
  flewrite(File, B, 0), !.
w_code_seg(byte(I, x(_)), File) :-
  flewrite(File, I, 0), !.
w_code_seg(byte(I, y(_)), File) :-
  flewrite(File, I, 0), !.
*/

w_code_seg(byte(B), File) :-
  flewrite(File, B, 0), !.
w_code_seg(xy(I, x(_)), File) :-
  flewrite(File, I, 1), !.
w_code_seg(xy(I, y(_)), File) :-
  flewrite(File, I, 1), !.
w_code_seg(arity(A), File) :-
  flewrite(File, A, 1), !.
/*  
w_code_seg(xindex(I), File) :-
  (
     I > 254 ->
     throw(comperr($\nToo many arguments, must be < 255$)) ;

     true
  ),
  (I = -1 -> flewrite(File, 255, 0) ;  flewrite(File, I, 0)), !.
*/

% NOTE!  This is all a very tricky business.  An xindex of 0
% means a temporary variable.  Since the beginning of time
% that has been the last XVar.  First would be better, but
% what a horrendous change...  So its last.  But this means
% maxvars CANNOT be set in the engine.  It has to be hard
% wired, and the number is 4096, so the last one is 4095 (x0fff).
% Why check -1? because we subtract one before getting here.

w_code_seg(xindex(I), File) :-
  (
     I >= 0x0fff ->
     throw(comperr($\nToo many x arguments, must be < 4095 $))
     ;
     true
  ),
  (I = -1 ->
     flewrite(File, 0x0fff, 1)
     ;
     flewrite(File, I, 1) ),
  !.
  
% ys are a different story.  oddness here is the 0x8000 bit
% might be on, signaling unsafe value, set by reg_ri, so we
% put that special case in uindex instead.
  
w_code_seg(yindex(I), File) :-
  (
     I  >= 0x0fff ->
     throw(comperr($\nToo many y arguments, must be < 4095 $))
     ;
     true
  ),
  (I < 0 ->
     throw(comperr(`negative (-1) yindex`))
     ;
     flewrite(File, I, 1) ),
  !.
  
w_code_seg(uindex(I), File) :-
  flewrite(File, I, 1),
  !.

w_code_seg(atom(A), File) :-
  flewrite(File, A, 1), !.
w_code_seg(int(I), File) :-
  flewrite(File, I, 1), !.
w_code_seg(label(I), File) :-
  flewrite(File, I, 1), !.
w_code_seg(cell(long_real, S), File) :-
  flewrite(File, 8, 0),                       % amark for reals 
  write_real(File, S), !.                     % write real directly 
w_code_seg(cell(string, S), File) :-          % strings are embedded in code 
  (is_unicode -> flewrite(File, 6, 0) ;  flewrite(File, 3, 0)),
  write_string(File, S), !.
w_code_seg(cell(double_float, F), File) :-    % floats are embedded in code 
  flewrite(File, 7, 0),                       % amark for floats 
  flewrite(File, F, 4), !.                    % write float directly 
w_code_seg(cell(single_float, F), File) :-    % floats are embedded in code 
  flewrite(File, 4, 0),                       % amark for floats 
  flewrite(File, F, 2), !.                    % write float directly 
w_code_seg(cell(integer, F), File) :-         % longs are embedded in code 
  flewrite(File, 5, 0),                       % amark for longs 
  flewrite(File, F, 3), !.                    % write long directly 
w_code_seg(cell(fixed_real, F), File) :-      % fixed are embedded in code 
  flewrite(File, 9, 0),                       % amark for fixed
  flewrite(File, F, 5), !.                    % write fixed directly 
%w_code_seg(cell(int, F), File) :-             % shorts not used are embedded in code 
%  flewrite(File, 2, 0),                       % amark for int
%  flewrite(File, F, 1), !.                    % write fixed directly 
w_code_seg(cell(atom, F), File) :-            % fixed are embedded in code 
  flewrite(File, 1, 0),                       % amark for atom
  flewrite(File, F, 1), !.                    % write fixed directly 
/*w_code_seg(cell(Type, I), File) :-
   (
      Type = int,
      Tag = 2 ;                                % consT-intS

      Type = atom,
      Tag = 1
   ),
   flewrite(File, Tag, 0),
   flewrite(File, I, 1), !.
*/
w_code_seg(raw(I), File) :-
  flewrite(File, I, 1), !.
w_code_seg(S, File) :-
  throw(comperr($\nCannot write code segment $, S)).

% report($\nCannot write code segment $), report(S), report($\n$).

write_real(File, Real) :-                     % write out array of gigits
%  nth(0, Real, Descr),
%  realDescr(Descr, Length, Exp, Sign, _),

% flow(mles:Length-Exp-Sign),
  real_components(Real, SignAtom, Exp, Length, Gigits),
%nl,writeq(Real),nl,
%write(SignAtom:Exp:Length),nl,
%write(Gigits),nl,
  flewrite(File, Length, 1),                  % 1 means write short
  flewrite(File, Exp, 1),
  atom_codes(SignAtom, [SignCode]),
  flewrite(File, SignCode, 1),
  write_gigits(File, Gigits).
  %for(I, 1, Length, 1),
  %nth(I, Real, Nth),
  %flewrite(File, Nth, 3),                     % write a long
  %I == Length.

write_gigits(File, []).
write_gigits(File, [G|Gs]) :-
   flewrite(File, G, 3),
   write_gigits(File, Gs).

write_string(File, String) :-                 % write out list of integers
  is_unicode,
  string_list(String, List),
  member(L, List),                            % to write string constants
%pre-unicode flewrite(File, L, 0),
  flewrite(File, L, 1),
  fail.
write_string(File, String) :-                 % write out list of integers
  not(is_unicode),
  string_list(String, List),
  member(L, List),                            % to write string constants
  flewrite(File, L, 0),
  fail.
write_string(File, _) :-                      % terminating null
%pre-unicode flewrite(File, 0, 0).
  (is_unicode -> flewrite(File, 0, 1) ;  flewrite(File, 0, 0)).

% object code for ops 

obj_code(no_op, 0).

% gets 
obj_code(get_var(x(_)), 31).
obj_code(get_var(y(_)), 35).
obj_code(get_val(x(_)), 47).
obj_code(get_val(y(_)), 48).
obj_code(get_con, 1).   % is this used?
obj_code(get_nil, 2).
obj_code(get_struc, 3).
obj_code(get_list, 4).

% puts 
obj_code(put_var(x(_)), 49).
obj_code(put_var(y(_)), 50).
obj_code(put_val, 35).
obj_code(put_unsafe, 5).
obj_code(put_con, 6).    % is this used?
obj_code(put_nil, 7).
obj_code(put_struc, 8).
obj_code(put_list, 9).
obj_code(put_val(x(_)), 44).
obj_code(put_val(y(_)), 45).

% sequencing codes 
obj_code(call, 10).
obj_code(mod_call, 55).
obj_code(proceed, 11).
obj_code(execute, 12).
obj_code(mod_execute, 56).
obj_code(escape, 13).
obj_code(call_local, 40).
obj_code(exec_local, 41).
obj_code(label, 37).
obj_code(allocate, 14).
obj_code(deallocate, 15).
obj_code(cut, 16).
obj_code(cutd, 17).
obj_code(cut64, 57).
obj_code(try_me_else, 18).
obj_code(try_me_or_else, 54).
obj_code(try, 19).
obj_code(retry_me_else, 20).
obj_code(retry, 21).
obj_code(fail, 32).
obj_code(trust_me_2_else, 33).
obj_code(trust_me_else, 22).
obj_code(trust, 23).
obj_code(switch_on_term, 27).
obj_code(goto, 28).
obj_code(switch_on_constant, 29).
obj_code(switch_on_structure, 30).

% unify ops 
obj_code(unify_var(x(_)), 36).
obj_code(unify_var(y(_)), 46).
obj_code(unify_unsafe, 38).
obj_code(unify_con, 24).
obj_code(unify_void, 25).
obj_code(u_var_getlist, 39).
obj_code(unify_nil, 26).
obj_code(unify_val(x(_)), 42).
obj_code(unify_val(y(_)), 43).

compute_lab(X, Xx) :-
  (X == fail, Xx = 0 ;  Xx = X), !.

%----------------------------------------------------------------------------%
% Local utils

abs(X, X) :-
  X >= 0, !.
abs(X, Y) :-
  Y is - X.





