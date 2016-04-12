%-*-Prolog-*-  
% compile indented on 12/23/2001 by 'JOLI' 1.0.

%------------------------------------------------------------------------
%
% compile.pro -- control flow for procedure compiler
%
% Copyright (c) 1992-2012 Amzi! inc. All Rights Reserved.
%
%------------------------------------------------------------------------

:- module(amzi_compiler).
:- import(amzi_register).
:- end_module(amzi_compiler).

:- body(amzi_compiler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compile one file %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% InFile is the source file, Outfile is the listing file (if any) and
% CodeFile is the generated object file

% Note that this can be called from another program, not
%  cmain.pro, in which case the listing option is turned off
%  by making OutFile = null.

% NOTE This requires aosutils to be loaded. THIS IS AN ERROR.
% This is only used by Eclipse and is bad form and should be rewritten.
xcompile(Dir, InFile, CodeFile, OutFile) :-
  curdir(CurDir),
  (chdir(Dir) ->
     true
     ;
     throw(comperr(`Unable to change directory`, Dir)) ),
     xcompile(InFile, CodeFile, OutFile).
     
% This throw does not work. Wraps error in an error structure instead of
% maintaining its original pieces that can be retrieved via ls.GetLineno, etc.
%  catch( xcompile(InFile, CodeFile, OutFile), Err, (chdir(CurDir), throw(Err)) ),
%  chdir(CurDir).
  
xcompile(InFile, CodeFile, OutFile) :-
  %buginit,
  (OutFile = null -> retractall(list_on) ;  assert(list_on)),
  %retractall(wamout(_)),
  abolish(wamout/1),
  compile(InFile, CodeFile, OutFile),
  clean_house.

compile(InFile, CodeFile, OutFile) :-
   clean_house,
%   abolish(input_clause/1),
%   retractall(last_fa(_)),
   read_input_file(InFile),
   compile2(CodeFile, OutFile).

clean_house :-
   abolish(input_clause/1),
   abolish(last_fa/1),
   abolish(is$dynamic/1),
   abolish(ops_defined/0).

% NOTE This requires aosutils to be loaded. THIS IS AN ERROR.
% This is only used by Eclipse and is bad form and should be rewritten.

debug_compile(Dir, InFile, CodeFile, OutFile) :-
  curdir(CurDir),
  (chdir(Dir) ->
     true
     ;
     throw(comperr(`Unable to change directory`, Dir)) ),
  debug_compile(InFile, CodeFile, OutFile).
     
% This throw does not work. Wraps error in an error structure instead of
% maintaining its original pieces that can be retrieved via ls.GetLineno, etc.
%  catch( debug_compile(InFile, CodeFile, OutFile), Err, (chdir(CurDir), throw(Err)) ),
%  chdir(CurDir).

debug_compile(InFile, CodeFile, OutFile) :-
   abolish(input_clause/1),
   retractall(last_fa(_)),
   amzi_system:debug_compile_read(InFile),
   compile2(CodeFile, OutFile).

%------------------------------------------------------------------
% First read all of the clauses and store them in input_clause/1
% clauses.  These are then backtracked through for the main compile
% loop.  This allows us to use alternate readers for processing the
% input file.
%

/*
read_input_file(InFile) :-
   (open(InFile, read, InHndl) ;  file_cannot_open(InFile)),
   repeat,
   read(InHndl, Clause),
   ( Clause = (:-(include(File2a))) ->
       tilt_slashes(File2a,File2),
       read_input_file(File2)
       ;
       true ),
   amzi_system:check$term(Clause),
   add_clause(Clause),
   Clause == end_of_file,
   !,
   close(InHndl).
*/

read_input_file(InFile) :-
   (open(InFile, read, InHndl) ;  file_cannot_open(InFile)),
   repeat,
   read(InHndl, Clause),
   ( Clause = (:-(include(File2a))) ->
       tilt_slashes(File2a,File2),
       read_input_file(File2)
       ;
       amzi_system:check$term(Clause),
       add_clause(Clause) ),
   Clause == end_of_file,
   !,
   close(InHndl).

/*
read_input_file(InFile) :-
   (open(InFile, read, InHndl) ;  file_cannot_open(InFile)),
   repeat,
   read(InHndl, Clause),
   ( Clause = (:-(include(File2a))) ->
       tilt_slashes(File2a,File2),
       read_input_file(File2)
       ;
       true ),
   (Clause == end_of_file ->
       true
       ;
       amzi_system:check$term(Clause),
       add_clause(Clause),
       fail ),
   !,
   close(InHndl).
*/

add_clause(Clause) :-
   get_fa(Clause, FA),
   ( FA == directive -> reader_directive(Clause); true),
   ( (predicate_end(FA, OLD_FA), OLD_FA \= end_of_file/0) ->
       assert( input_clause(end_of_predicate(OLD_FA)) )
       ;
       true ),
   assert( input_clause(Clause) ).


% directives that need to be dealt with now because they
% affect the reader
reader_directive(:-(op(A, B, C))) :-
   !,
   op(A, B, C).
reader_directive(:-(set_prolog_flag(F, V))) :-
   !,
   set_prolog_flag(F, V).
reader_directive(_).
   
get_fa( (:- _), directive) :-
   !.
get_fa( (Head :- _), F/A ) :-
   !,
   functor(Head, F, A).
get_fa( (Head --> _), F/A) :-
   !,
   functor(Head, F, A1),
   A is A1 + 2.
get_fa( Head, F/A ) :-
   functor(Head, F, A).

% not a new one, same as last one
predicate_end(FA, _) :-
   last_fa(FA),
   !,
   fail.
% there was an old one and its different
predicate_end(FA, OLD_FA) :-
   retract(last_fa(OLD_FA)),
   !,
   assert(last_fa(FA)),
   OLD_FA \= directive.
% there wasn't an old one, so this is first clause
predicate_end(FA, _) :-
   assert(last_fa(FA)),
   !,
   fail.

%--------------------------------------------------

%compile(InFile, CodeFile, OutFile) :-
compile2(CodeFile, OutFile) :-
  (list_on -> (tell(OutFile) ;  file_cannot_open(OutFile)) ;  true),
%(see(InFile) ; file_cannot_open(InFile)),
%(fopen(InHndl, InFile, r) ; file_cannot_open(InFile)),
%  (open(InFile, read, InHndl) ;  file_cannot_open(InFile)),
  retractall(dyn$fact(_, _)),
  retractall(is$discontiguous(_:_/_)),
  retractall(is$dynamic(_:_/_)),
  retractall(ops_defined), !,

              %(fleopen(CodeHndl, CodeFile, wb) ; file_cannot_open(CodeFile)),
  (
     open(CodeFile, write, CodeHndl, [type(binary)]) ;

     file_cannot_open(CodeFile)
  ),
  reserve$(AssemblePoint),                    % to stash code for assembling
  reserve$(ClausePoint),                      % to stash cross clause info
  reserve$(CurrentProc),                      % to stash current proc info
  write_code_header(CodeHndl),
  Time1 is cputime,
  process_clauses(OutFile, CodeHndl, InHndl, AssemblePoint, ClausePoint, 
                 CurrentProc),
  Time2 is cputime, !,
  fseek(CodeHndl, 0, 1, NumBytes),            % seek to EOF for size 
  write_l($\n[CodeSize $),
  write_l(NumBytes),
  write_l($ Bytes, Compile time $),
  Time is Time2 - Time1,
  write_l(Time),
  write_l($ seconds.]\n$),
  (list_on -> told ;  true),
  fseek(CodeHndl, 0, 2, _),                   % add two 0s at end for VMS bug
  flewrite(CodeHndl, 0, 1),                   %fclose(CodeHndl), 
  close(CodeHndl),
  stash$free(AssemblePoint),
  stash$free(ClausePoint),
  stash$free(CurrentProc),
  seen,
  !.
compile2(_, _) :-
  report(`Compiler failed\n`).

% The compiler proper. In order to compile VERY large procedures
% (potentially hundreds of clauses) we take a different approach from
% that in versions 1.0X and 1.1X.
%
% We compile a single clause at a time and write it out. We then go
% back at the end of the procedure (procedures must still be contiguous)
% And add any hash tables required. This means that we have to compile
% in some redundancies that we can let the linker and loader resolve:
%
% The first clause of a procedure has to begin with
% switch A, B, C
% try_me_else
%
% And succeeding clauses have to begin with
% retry_me_else
%
% We then CLOSE the procedure at the end. This means we
% Fill in all the correct addresses for the inserted
% (re)try_me_else
%
% Add any hash tables required and fix up the initial
% switch A, B, C
%
% In order to acomplish this we need to track certain information
% for each clause in the procedure - this information has to be available
% to the procedure closer, so we call this cross clause info. In particular
% we need
%
% The physical disk address of the first code bye in each clause
% of the procedure - so we can get back during the link up phase
%
% The number of the clause to be used as a label for branching
%
% The first argument of each clause (if there is one) for hash table
% generation
%
% The inserted switch and first try_me_else are initially written out
% as 0s. Then if there is more than one clause (and the procedure closer
% decides that the switch is needed) the 0 are overwritten with proper code.
%
% This means that the loader and linker can decide at load time where the real
% code starts (it will either be at the switch, at the try_me_else or at the
% next op code if there is only one clause. Moreover since this possible
% trimming takes place at the prefix to the code sgment, a simple linear
% translation on all the op codes with a code address in them will suffice.
% Once the code has been loade in this fashion it is indistinguishable from
% any other code

process_clauses(OutFile, CodeHndl, InHndl, AP, CP, CurrProc) :-
  cntr_set(1, 1),          % counter 1 is used to track clauses
  cntr_set(3, -1),         % counter 3 tracks labels, negatives during compile
%  repeat,
%  read(InHndl, Clause),
  input_clause(Clause),
%  write( input_clause(Clause) ), nl,
  amzi_system:check$term(Clause),
/*
  ( Clause == end_of_file ->               % Link up the last procedure
      close_procedure(CodeHndl, CP, CurrProc)
      ;
      ( Clause = (:-(include(File2a))) ->
          tilt_slashes(File2a, File2),
          ( open(File2, read, InHndl2) ->
              process_clauses(OutFile, CodeHndl, InHndl2, AP, CP, CurrProc),
              close(InHndl2),
              fail
              ;
              file_cannot_open(InFile) )
          ;
*/
  compile_a_clause(Clause, OutFile, InHndl, CodeHndl, AP, CP, CurrProc),
  !.
process_clauses(_,_,_,_,_,_).

compile_a_clause(end_of_file, OutFile, InHndl, CodeHndl, AP, CP, CurrProc) :-
   !, fail.
compile_a_clause(end_of_predicate(N/A), OutFile, InHndl, CodeHndl, AP, CP, CurrProc) :-
   check_dynamic(N/A),  % was really a latent in that case
   !, fail.
compile_a_clause(end_of_predicate(N/A), OutFile, InHndl, CodeHndl, AP, CP, CurrProc) :-
   ( (loading_module(M), clause(is$discontiguous(M:N/A), true)) ->
         functor(F, N, A),
         compile_clause(N/A, _, (F :- fail), OutFile, CodeHndl, AP, CP, CurrProc)
         ;
         true
   ),
%   write(closing_predicate(N/A)), nl,
   close_procedure(CodeHndl, CP, CurrProc),
   !,
   fail.

compile_a_clause(Clause, OutFile, InHndl, CodeHndl, AP, CP, CurrProc) :- 
   filter_clause(Clause, FC, N/A, FArg),
/*   peek$(CurrProc, CPI), 
   ( (CPI == []; CPI == [latent_exp/0]; CPI == [latent_opdef/0]) ->       % First clause of predicate
      stash$(CurrProc, (N/A))
      ;
/.*
    	% If compiling a new proc or proc is a latent
    	% expression then close and Link the old proc
      ( (CPI \== [N/A]; CPI == [latent_exp/0]; CPI == [latent_opdef/0]) ->
           close_procedure(CodeHndl, CP, CurrProc),
           X = newProc,
           stash$(CurrProc, (N/A))            % And assert new current proc
           ;
*./
       true                                  % not a new proc
   ),
*/
   ((loading_module(M), clause(is$discontiguous(M:N/A), true)) ->    % check directives
/*
      %fseek(InHndl, 0, 1, BookMark),           % discontiguous or multifile
      stream_property(InHndl, position(BookMark)),
      read(InHndl, NextClause),                % look ahead
      ( NextClause \= end_of_file -> 
      	%fseek(InHndl, BookMark, 0, BookMark)
      	set_stream_position(InHndl, BookMark)
      	;
      	true
      ),    % go back
      filter_clause(NextClause, _, N1/A1, _),
      ( (N1/A1 \= N/A) ->                       % is it same proc?
         functor(F, N, A),              % no, this is last, compile extra clause
         compile_clause(N/A, _, (F :- fail), OutFile, CodeHndl, AP, CP, CurrProc)
         ;
*/
       ((cntr_get(1,ClauseN), ClauseN > 1000) ->
           functor(F, N, A),              % no, this is last, compile extra clause
           compile_clause(N/A, _, (F :- fail), OutFile, CodeHndl, AP, CP, CurrProc),
           close_procedure(CodeHndl, CP, CurrProc),
           cntr_set(1,1)
           ;
           true )
       ;
       true ),
       
   compile_clause(N/A, FArg, FC, OutFile, CodeHndl, AP, CP, CurrProc),
   ((N/A == latent_exp/0; N/A == latent_opdef/0) ->
       close_procedure(CodeHndl, CP, CurrProc)
       ;
       true ),
   !, fail.

% Compile clauses - stash code at AP

compile_clause(N/A, FArg, Clause, OutFile, _, AP, CP, CurrProc) :-
  % Set current proc if first clause of proc
  (peek$(CurrProc, [(N/A)]) -> true ;  stash$(CurrProc, (N/A))),
  compileclause(Clause, Code - L),
  L = [],
  stash$(AP, Code),
  fail.                                       % Now assemble and we're done
compile_clause(N/A, FArg, _, _, CodeHndl, AP, CP, _) :-
  peek$(CP, CPInfo),
  is_it_first(CPInfo, FirstClause),
  get$(AP, [Code]),
  output(CodeHndl, Code, N/A, FirstClause, PhysicalP),

                                              % Now update cross clause info
  cntr_inc(1, Label),                         % Get clause # and increment
  stash$(CP, cpinfo(FArg, Label, PhysicalP)), !.

is_it_first([], yes).
is_it_first([_|_], no).

/*********************************************************************
**  filter a clause -- source - source pre translations
*********************************************************************/

filter_clause(?-(X), _, _, _) :-
  (call(X) ;  true), !,  % ?- is a compile time directive, it is not compiled 
  fail.

/*   :- compiles to a latent expression.
**   it is not executed at compile time, except when the body is:
**            an op() directive,
**            a dynamic directive,
**            import/export
*/
filter_clause(:-(nonterminal(NonTerminalList)), _, _, _) :-
  add_nonterminal_list(NonTerminalList), !,
  fail.
filter_clause(:-(dynamic(DynamicList)), _, _, _) :-
  add_dynamic_list(DynamicList),
  !,
  fail.
filter_clause(:-(noNonTerminals), _, _, _) :-
  sys$assertz('{sys}no$nt'), !,
  fail.
filter_clause(:-(discontiguous(DL)), (latent_exp :- amzi_system:set$discontiguous(DL)), 
             latent_exp/0, _) :-
  add_discontiguous_list(DL), !.
filter_clause(:-(multifile(DL)), (latent_exp :- amzi_system:set$discontiguous(DL)), 
             latent_exp/0, _) :-
  add_discontiguous_list(DL), !.
filter_clause(:-(sorted(DL)), (latent_exp :- amzi_system:set$sorted(DL)), latent_exp/0, 
             _) :-
  add_dynamic_list(DL), !.
filter_clause(:-(indexed(DL)), (latent_exp :- amzi_system:set$$indexed(DL)), 
             latent_exp/0, _) :-
  add_dynamic_list(DL), !.
filter_clause(:-(op(A, B, C)), (latent_opdef :- op(A, B, C)), 
             latent_opdef/0, _) :- !,
  op(A, B, C),
  (clause(ops_defined) -> true ;  assert(ops_defined)).
filter_clause(:-(set_prolog_flag(F, V)), (latent_exp :- set_prolog_flag(F, V)), 
             latent_exp/0, _) :- !,
  set_prolog_flag(F, V).

% Need to set up the module stuff as we go, so that the pretranslate
% routines for metapredicates will work, see cclause, xpretrans.
filter_clause(:-(module(M)), (latent_exp :- module$(M)), latent_exp/0, _) :-
  module$(M), !.
filter_clause(:-(end_module(M)), (latent_exp :- end_module$(M)), 
             latent_exp/0, _) :-
  end_module$(M), !.
filter_clause(:-(body(M)), (latent_exp :- module$(M)), latent_exp/0, _) :-
  module$(M), !.
filter_clause(:-(end_body(M)), (latent_exp :- end_module$(M)), 
             latent_exp/0, _) :-
  end_module$(M), !.
filter_clause(:-(metapredicate(MI)), (latent_exp :- amzi_system:meta$assert(MI)), 
             latent_exp/0, _) :-
  amzi_system:meta$assert(MI), !.
filter_clause(:-(Body), (latent_exp :- Body), latent_exp/0, _) :- !.
filter_clause((A --> B), DCG, Name/Ar, FArg) :- % use DCG compiler 
  expand_term((A --> B), DCG),
  functor(A, Name, Arity),
  first_arg(A, FArg),
  Ar is Arity + 2, !.
filter_clause((H :- B), (latent_exp :- Body), latent_exp/0, _) :- % reeves
  functor(H, N, A),
  check_dynamic(N/A),
  %clause(is$dynamic(N/A), true),
  Body = assertz((H :- B)), !.
filter_clause((H :- B), (H :- B), N/A, FArg) :-
  functor(H, N, A),
  first_arg(H, FArg), !.
filter_clause(H, (latent_exp :- Body), latent_exp/0, _) :- % reeves
  functor(H, N, A),
  check_dynamic(N/A),
  %clause(is$dynamic(N/A), true),
  Body = assertz(H), !.
filter_clause(H, H, N/A, FArg) :-
  functor(H, N, A),
  first_arg(H, FArg).

% first_arg extracts the first argument from the clause - atomic
% terms and variables are themselves, functors are represented as
% N/A where a list is '.'/2

first_arg(Head, FArg) :-
  Head =.. [_, Arg1|_],
  farg(Arg1, FArg), !.
first_arg(_, _).

farg(Arg1, _) :-
  var(Arg1), !.
farg(Arg1, Arg1) :-
  atomic(Arg1), !.
farg([_|_], '.'/2) :- !.
farg(Arg1, Struc/Arity) :-
  functor(Arg1, Struc, Arity).

add_discontiguous_list(N/A) :-
  atom(N),
  integer(A), !,
  loading_module(M),
  ( clause(is$discontiguous(M:N/A), true) ->
      true
      ;
      assert(is$discontiguous(M:N/A)) ).
add_discontiguous_list([]).
add_discontiguous_list([H|T]) :-
  add_discontiguous_list(H),
  add_discontiguous_list(T).
add_discontiguous_list((A, B)) :-
  add_discontiguous_list(A),
  add_discontiguous_list(B).
add_discontiguous_list(X) :-
  throw(comperr(badglobal, X)).

check_discontiguous(N/A) :-
  loading_module(M),
  clause(is$discontiguous(M:N/A), true),
  !.

add_dynamic_list([]).
add_dynamic_list(N/A) :-
  atom(N),
  integer(A), !,
  loading_module(M),
  assert(is$dynamic(M:N/A)).
add_dynamic_list((A, B)) :-
  add_dynamic_list(A),
  add_dynamic_list(B).
add_dynamic_list([H|T]) :-
  add_dynamic_list(H),
  add_dynamic_list(T).
add_dynamic_list(P) :-
  functor(P, N, A), !,
  loading_module(M),
  assert(is$dynamic(M:N/A)).
add_dynamic_list(X) :-
  throw(comperr(badglobal, X)).
  
check_dynamic(N/A) :-
  loading_module(M),
  clause(is$dynamic(M:N/A), true),
  !.

add_nonterminal_list([]).
add_nonterminal_list(N/A) :-
  atom(N),
  integer(A), !,
  assert(is$nonterminal(N/A)).
add_nonterminal_list((A, B)) :-
  add_nonterminal_list(A),
  add_nonterminal_list(B).
add_nonterminal_list([H|T]) :-
  add_nonterminal_list(H),
  add_nonterminal_list(T).
add_nonterminal_list(X) :-
  throw(comperr(badglobal, X)).

/*  write a header block to the code file --
**      8 bytes as follows
**
**      byte 1 set to 0xff  -- so we can sense if the file is not ascii
**      byte 2 set to 0x03 -- the new multilinked object file tag - indicating
**            .plm file.  Anded with 0x10 to indicate Unicode.
**      byte 3 -- version
**      byte 4 -- build
**      bytes 5-8 set to 0
*/

write_code_header(CodeHndl) :-
  flewrite(CodeHndl, -1, 0),                  % byte 1
  (
     is_unicode ->
     flewrite(CodeHndl, 0x13, 0) ;

     flewrite(CodeHndl, 0x03, 0)
  ),
  version_build(V, B, _),
  flewrite(CodeHndl, V, 0),                   % byte 3 version
  flewrite(CodeHndl, B, 0),                   % byte 4 build
  flewrite(CodeHndl, 0, 0),
  flewrite(CodeHndl, 0, 0),
  flewrite(CodeHndl, 0, 0),
  flewrite(CodeHndl, 0, 0), !.                % byte 8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code to string clauses together
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close_procedure(CodeHndl, ClausePoint, CurrProc) :-
  get$(ClausePoint, CP),
  get$(CurrProc, [N/A]),

      % see whether we need to string the clauses together along the var block
  (
     CP = [cpinfo(_, _, PP)] ->               % Only one clause
     fseek(CodeHndl, 0, 1, EOF),
     PP1 is PP - 6,
     fseek(CodeHndl, PP1, 0, _),              % Seek to clause info int
     flewrite(CodeHndl, 1, 1),                % set to last cls 0x0001
     fseek(CodeHndl, PP, 0, _),               % Get to the switch
     flewrite(CodeHndl, 52, 0),               % set to no_switch op code
     fseek(CodeHndl, 6, 1, _),                % Get to the try_me_else
     flewrite(CodeHndl, 53, 0),               % set to no_try op code
     fseek(CodeHndl, EOF, 0, _) ;

% Else more than one clause so start the string process

     string_clauses(CodeHndl, N, A, CP)
  ),
  cntr_set(1, 1),       % clause numbers and labels are relative to procedures
  cntr_set(3, -1).
close_procedure(_, _, _) :-
  write_l($Error: Unable to close procedure\n$).

% String clauses links up the clauses along the variable block
% (by writing the label fields of the try_me_elses) and then
% generates any branch tables at the end of the procedure
% For 3.0, the relative offsets are no longer stored in the try instructions,
% instead, simply the clause number of where to jump to is stored. This way
% the loader can decide what the right offset is for the target machine.
% This quick fix here should be improved by removing the other code in the
% system that is designed to support the relative offset concept.
% Branch tables are added by just writing another clause for the procedure
                                              %

string_clauses(Hndl, N, A, [cpinfo(FArg, Label, PP)|T]) :-

                         % String together clauses along their variable blocks
                                              % Special case first clause
  fseek(Hndl, 0, 1, EOF),                     % where we will build hashes
  FirstTry is PP + 8,                         % skip switch, try opcode
  fseek(Hndl, FirstTry, 0, _),                % move to label of try
  NextLabel is Label + 1,
  flewrite(Hndl, NextLabel, 1),               % simply write # of next clause
  string_v_clauses(Hndl, T, LastPP),          % Now chain rest of clauses
  fseek(Hndl, EOF, 0, _),                     % Move back to EOF
  build_branch(Hndl, N, A, [cpinfo(FArg, Label, PP)|T], LastPP). %

string_v_clauses(Hndl, [cpinfo(_, _, PP)], PP1) :- !, % We are last clause
  fseek(Hndl, PP, 0, _),                      % Get to retry_me_else op code
  flewrite(Hndl, 33, 0),                      % Chng to trust_me_2_else_fail
  PP1 is PP - 6,
  fseek(Hndl, PP1, 0, _),                     % Get to info word in clause
  flewrite(Hndl, 1, 1).                       % Change to 1 ("last clause")
                                              %
string_v_clauses(Hndl, [cpinfo(FArg, Label, PP)|T], LastPP) :-
  NextTry is PP + 1,                          % Get to label of try_me op
  fseek(Hndl, NextTry, 0, _),
  NextLabel is Label + 1,
  flewrite(Hndl, NextLabel, 1),               % simply write # of next clause
  string_v_clauses(Hndl, T, LastPP).          % Now chain rest of clauses
% Build the branch tables starting at current posn in codefile

build_branch(H, _, 0, CCI, _) :-              % Arity 0 -> no branch tables
  kill_switch(H, CCI), !.
build_branch(H, N, A, CCI, _) :-              % All first args are variable
  all_var(CCI),                               % -> no branch tables
  kill_switch(H, CCI), !.
build_branch(H, N, A, CCI, LastPP) :-

                  % Otherwise generate hash tables and wire the initial switch
  do_blocks(CCI, A, [C, L, S], Code),

% Some or all of Const, List, Struc may currently be variables
% (Branch tables whose code exists but which has not yet been allocated). 
% So, first we write the code, then we fix the switch.
  (
     Code \= [] ->
     output(H, Code, N/A, no, PP),

% LastPP has LastClause marker of what was the last clause
% since it is no longer the last clause (Code is)
% we reset to no last clause and set Code to last clause
     fseek(H, 0, 1, EOF),
     fseek(H, LastPP, 0, _),
     flewrite(H, 0, 1),
     PP1 is PP - 6,
     fseek(H, PP1, 0, _),
     flewrite(H, 1, 1) ;

     fseek(H, 0, 1, EOF)
  ),
  CCI = [cpinfo(_, _, StartCode)|_],          % Get to beginning of procedure
  fseek(H, StartCode, 0, _),                  % Make the switch real
                                            % Compute relative jmps for switch
  switch_rel_jmp(C, C1),
  switch_rel_jmp(L, L1),
  switch_rel_jmp(S, S1),
  flewrite(H, 27, 0),                         % Switch op code
  flewrite(H, C1, 1),                         % And the labels
  flewrite(H, L1, 1),
  flewrite(H, S1, 1),
  fseek(H, EOF, 0, _), !.

switch_rel_jmp(0, 0) :- !.
switch_rel_jmp(fail, 0) :- !.
switch_rel_jmp(B, B).

kill_switch(H, [cpinfo(_, _, S)|_]) :-
  fseek(H, 0, 1, EOF),
  fseek(H, S, 0, _),
  flewrite(H, 52, 0),                         % write the no_switch op code
  fseek(H, EOF, 0, _).

/*
  do_blocks( CCI, Arity, [Const, List, Struc], Code)

  CCI is our cross clause info
  [Const, List, STruc] are the corresponding labels for the switch
  Code is a list of branch codes (may be empty)
*/

% only vars and one other kind in first args - 
% if X1 is of the other kind it can only match with the vars 

do_blocks(CCI, Arity, CLS, TryCode) :-
  same_or_var(CCI, Kind), !,     % fails or sets Kind to one other kind of arg
  filterv(CCI, VarC),        % VarC is subset of CCI with all variable clauses
  try_block(VarC, Arity, TryLbl, TryCode, TryLink), % Brnch table for Kind
  block_code((Kind, CCI), Arity, TryLbl, 1, CLS, TryLink, []).

/* else generate all blocks
   note that the vars, ..Lbl, are bound to vars 
   in Code of the form label(..Lbl) 
*/
do_blocks(CCI, Arity, [ConstLbl, ListLbl, StrucLbl], Code) :-
  filterlcs(CCI, ListC, ConstC, StrucC),
  try_block(ListC, Arity, ListLbl, Code, LLink),
  cs_block(ConstC, Arity, ConstLbl, LLink, CLink, _),
  cs_block(StrucC, Arity, StrucLbl, CLink, [], _).

% 1st arg is list or var

block_code((list, _), _, TryLbl, VarLbl, [TryLbl, VarLbl, TryLbl], L, L) :- !.

             % VarLbl is top of the var block chain - always after the switch.
                                             % else 1st arg is struct or const
block_code((Kind, CompC), Arity, TryLbl, VarLbl, CLS, Code, Link) :-
  cs_block(CompC, Arity, BlkLbl, BlkCode, BlkLink, Hashed),
  (                                        % no_hash => no separate try_block 
     Hashed = no_hash ->
     CSLbl = VarLbl,                          % no hash
     Code = Link ;

     CSLbl = BlkLbl,                          % hash
     Code = BlkCode,
     Link = BlkLink
  ),
  (
     Kind = constant ->
     CLS = [CSLbl, TryLbl, TryLbl] ;          % constant

     CLS = [TryLbl, TryLbl, CSLbl]            % not constant
  ), !.

same_or_var([cpinfo(FArg, _, _)|Rest], Kind) :- % true if 1st args are all 
  kind(FArg, K),                              % variable and 1 other kind 
  (K = variable ;  K = Kind), !,
  same_or_var(Rest, Kind).
same_or_var([], _).

all_var(CompC) :-                             % true if 1st args all vars 
  same_or_var(CompC, variable).               %
/*
**     Filter clauses which could match with a list, const or struc as first
**     argument. Note that a variable as first arg matches with all of them.
*/

filterlcs([], [], [], []).
filterlcs([X|Rest], [X|ListLbls], [X|ConstLbls], [X|StrucLbls]) :-
  X = cpinfo(FArg, _, _),
  var(FArg), !,
  filterlcs(Rest, ListLbls, ConstLbls, StrucLbls).
filterlcs([X|Rest], [X|ListLbls], ConstLbls, StrucLbls) :-
  X = cpinfo('.'/2, _, _),
  filterlcs(Rest, ListLbls, ConstLbls, StrucLbls).
filterlcs([X|Rest], ListLbls, [X|ConstLbls], StrucLbls) :-
  X = cpinfo(FArg, _, _),
  atomic(FArg), !,
  filterlcs(Rest, ListLbls, ConstLbls, StrucLbls).
filterlcs([X|Rest], ListLbls, ConstLbls, [X|StrucLbls]) :-
  filterlcs(Rest, ListLbls, ConstLbls, StrucLbls).

filterv([], []).
filterv([X|Rest], [X|VarLbls]) :-       % Filter clauses with vars as 1st args
  X = cpinfo(FArg, _, _),
  var(FArg), !,
  filterv(Rest, VarLbls).
filterv([_|Rest], VarLbls) :-
  filterv(Rest, VarLbls).

/*
**    Try block -- generic try-block to try all clauses in the given list.
**    Optimizes only if 0 or 1 clauses are given
**
**      try_block(ListOfCrossClauses, NumberTempVars, Label, Code, Link)
**
**      Try_block produces a list of try/retry codes to branch to the 
**      clauses specified in the ListOfCrossClauses.
**      This code is contained at Label and is the code Code. 
**      The switch will branch to Label to try the clauses sequentially.
**
**      If there is no code in the CCI then  our label for the branch header
**      is simply 0 (the fail)
**      If there is exactly one clause in the CCI then our branch header label
**      itself. This means the switch will go directly to the clauses rather 
**      than generating a try list with only one element
*/

try_block([], _, 0, L, L).                    % /5 empty
try_block([cpinfo(_, Lbl, _)], _, Lbl, L, L) :- !. % singleton
try_block([cpinfo(_, Lbl, _)|Clauses], NTV, Label, Code, Link) :- % general
  try_block(Clauses, LCode, Link),            % go to try_block/3
  Code = [label(Label), try(NTV, Lbl)|LCode]. % 'try'

try_block([cpinfo(_, Lbl, _)], [trust(Lbl)|L], L) :- !. % /3 base 'trust'
try_block([cpinfo(_, Lbl, _)|Clauses], [retry(Lbl)|LCode], Link) :- % 'retry'
  try_block(Clauses, LCode, Link).            % recurse
/*
    **     Const and structure block: First arg is const or structure.
    **     This routine works for both consts and structs.
    **     Difference with try_block: generates hash tables if needed
    **     Variable Hashed indicates whether hash tables were generated -
    **     It is either no_hash or yes_hash
    */

cs_block([], _, fail, Link, Link, no_hash).
cs_block([cpinfo(_, Lbl, _)], _, Lbl, Link, Link, no_hash).
cs_block(Clauses, Arity, Lbl, [label(Lbl)|Code], Link, Hashed) :-
  cs_gather(Clauses, [], Gather, [], Hashed),
  (var(Hashed) -> Hashed = no_hash ;  true),
  cs_link(try, Arity, Gather, Code, Link).

/*
**    Gather contiguous arguments which are not variables together.
**    The other arguments are left separate
*/

cs_gather([X|Rest], Collect, Gather, Link, H) :-
  X = cpinfo(FArg, _, _),
  var(FArg), !,
  dump(Collect, Gather - G, H),
  G = [X|G2],
  cs_gather(Rest, [], G2, Link, H).
cs_gather([X|Rest], Collect, Gather, Link, H) :-
  X = cpinfo(FArg, _, _),
  member(cpinfo(FArg, _, _), Collect), !,
  dump(Collect, Gather - G, H),
  cs_gather(Rest, [X], G, Link, H).
cs_gather([X|Rest], Collect, G, L, H) :-
  cs_gather(Rest, [X|Collect], G, L, H).
cs_gather([], Collect, Gather, Link, H) :-
  dump(Collect, Gather - Link, H).

/*
    **    Convert a collection of clauses to a member of Gather
    **    If Collect is > 1, it (as list) is a member.
    **    Else just its element clause is member
    */

dump([], L - L, _).
dump([X], [X|L] - L, _) :-
  X = cpinfo(_, _, _).
dump(Collect, [Collect|L] - L, yes_hash).

cs_link(Type, Arity, [Gr], Code, Link) :-
  (              % Link all elements of Gather together with try, retry, trust
     Gr = cpinfo(_, Lbl, _) ->
     Code = [trust(Lbl)|Link] ;

     hash(Gr, Hash - Link),
     (Type = try -> Code = Hash ;  Code = [trust(else, fail)|Hash])
  ).
cs_link(try, NTV, [Gr|Rest], Code, Link) :- !,
  (
     Gr = cpinfo(_, Lbl, _) ->
     (Instr =.. [try, NTV, Lbl], Code = [Instr|L]) ;

     (
        Gr = [cpinfo(_, _, _)|_],
        hash(Gr, Hash - [label(ElseLbl)|L]),
        Code = [Instr|Hash],
        Instr =.. [try, NTV, else, ElseLbl]
     )
  ),
  cs_link(retry, NTV, Rest, L, Link).
cs_link(Type, NTV, [Gr|Rest], Code, Link) :-
  (
     Gr = cpinfo(_, Lbl, _) ->
     (Instr =.. [Type, Lbl], Code = [Instr|L]) ;

     (
        hash(Gr, Hash - [label(ElseLbl)|L]),
        Code = [Instr|Hash],
        Instr =.. [Type, else, ElseLbl]
     )
  ),
  cs_link(retry, NTV, Rest, L, Link).

hash(Gr, Code - Link) :-     % generate hash table with switch inst - cosmetic
  hash_table(Gr, HashTbl, Link, Fudge, HashLen),
  cs_kind(Gr, Kind),
  Code = [switch(Kind, HashLen, Fudge)|HashTbl].

cs_kind([cpinfo(FArg, _, _)|_], Kind) :- % if Gr is bunch of consts or structs
  kind(FArg, Kind).                         % no par needs passing to cs_block

hash_table([cpinfo(FArg, Lbl, _)|Rest],    % Construct hash table - dummy code
          [pair(FArg, Lbl)|Hash], Link, fudge, Len) :-
  hash_table(Rest, Hash, Link, _, Lenl),
  Len is Lenl + 1.
hash_table([], Link, Link, _, 0).

kind(Arg, variable) :-                        % Returns kind of argument 
  var(Arg), !.
kind(Arg, constant) :-
  atomic(Arg), !.
kind('.'/2, list) :- !.
kind(_, structure).

/*--------------------------------------------------------------------------*/

% Local utils

output(_, Code, N/A, _, _) :-                 % Analyze code *** Temp
  clause(wamout(H), true),                    % if wamout 
  numbervars(Code, 0, _),
  telling(X),
  tell(H),
  write_plm(N/A, Code),
  tell(X),
  fail.                                       % do next clause anyway
output(CodeFile, Code, N/A, First, PA) :-
  timer(T1),
  assemble(CodeFile, Code, N/A, First, PA),
  timer(T2).

read_list([H|T]) :-                % read a list from keyboard ignoring spaces
  eat_spaces(H),
  H \= 10,
  read_list(T), !.
read_list([]).

eat_spaces(X) :-
  (get0(X), X \= 0'  ;  eat_spaces(X)), !.

puts([]).
puts([H|T]) :-                                % print out a " string "
  put(H), 
  puts(T).

file_cannot_open(File) :-
  throw(comperr($\nCannot open file : \n$, File)).

% report($\nCannot open file : \n$),
% report(File),
% fail.
/* Write to listing window and also, optionally, to listing file (stdout) */

/* This seems dumb, so we've changed it to simply write to the listing file, 
   which might be sysout. */

write_l(Term) :-
  (list_on -> report(Term) ;  true).

% report(Term),
% telling(X),
% (list_on -> tell(user), report(Term), tell(X) ; true).

write_w(T) :-
  (list_on -> report(T) ;  true).

:- end_body(amzi_compiler).

