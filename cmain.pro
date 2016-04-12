%-*-Prolog-*-  
% cmain indented on 12/23/2001 by 'JOLI' 1.0.

%----------------------------------------------------------------------
%
% cmain.pro -- the compiler shell
%
% Copyright (c) 1992-2012 Amzi! inc. All Rights Reserved.
%
%----------------------------------------------------------------------

main$ :-
  amzi_compiler:main.

:- body(amzi_compiler).

greetings :-
   write(`\n  ****************************************************\n`), 
   write(`\n  Amzi! Prolog Compiler `),
   write(`\n  Version: `),
   version(Ver), write(Ver), nl,
   write(`  Copyright (c) 1992-2016 Amzi! inc.\n`),
   write(`  Open Source version licensed under the M.I.T. License.\n`),
   write(`  ****************************************************\n`).

main :-
   greetings,
   startmain.

% catch(startmain, X, process_exception(X)).

startmain :-
  user_inputs(Files, Options), 
  doit(Files, Options).

compile_file(FStr) :-
  greetings,
  string_list(FStr, FChl),
  catch(startcompile(FChl), X, process_exception(X)),
  farewells.

startcompile(FChl) :-
  opts_n_files(Options, Files, [FChl], []),
  apply_defaults(Files, [S, O, L]),
  doit([S, O, L], Options).

process_exception(comperr(E, Arg)) :-         % errors thrown by the compiler
  write($Compiler Error: $), 
  error_message(E, Arg), !,
  fail.
process_exception(comperr(E)) :-              % errors thrown by the compiler
  write($Compiler Error: $), 
  error_message(E), !,
  fail.
process_exception(amzierror(Err, Attrs)) :-   % errors thrown by the system
  write($Error During Compilation: $), 
  member(rc = RC, Attrs),
  member(type = TYPE, Attrs),
  member(message = MSG, Attrs),
  write(Err), tab(1), write(RC), nl, write(MSG), nl, 
  (
     TYPE == read ->
     member(read_buffer = RB, Attrs),
     write($Read buffer:\n$), write(RB), nl ;

     true
  ), !,
  fail.
process_exception(error(Error_term, Imp_def)) :- % ISO standard format
  isoError(Error_term, Imp_def),  %stream_attrs(AttrList), % available to user
                                              %stream$attrs(AttrList),
  fail.
process_exception(E) :-
  write($Exit with $), writeq(E), nl, 
  fail.

stream$attrs([]).
stream$attrs([stream(Kind, Name, Lino, Col)|Rest]) :- % recurse thru readers
  write(stream(Kind)), tab(1), write(Name),   % write stream attributes 
  write($ line: $), write(Lino), write($ column: $), write(Col), nl, 
  stream$attrs(Rest).

error_message(earlyop) :-
  errout($Must define imports and exports before any operators.$).
error_message(codetoolong) :-
  errout($This predicate is too big. Split the clauses into separate predicates.$).
error_message(atomstoolong) :-
  errout($This predicate's atom table is too long. Use strings where practical.$).
error_message(STRING) :-
  errout(STRING),
  nl.

error_message(badfilename, FileName) :-
  errout($Bad file name: $),
  errout(FileName),
  nl.
error_message(badglobal, X) :-
  errout($Bad import/export specification: $),
  errout(X),
  nl.
error_message(STRING, ARG) :-
  errout(STRING),
  errout(ARG),
  nl.

errout(X) :-
  write(X).

isoError(type_error(ValidType, Culprit), Imp_def) :-
  write($type error: $), write(Culprit), write($ is not $), 
  write(ValidType), nl.
isoError(domain_error(ValidDomain, Culprit), Imp_def) :-
  write($domain error: $), write(Culprit), write($ is not $), 
  write(ValidDomain), nl.
isoError(existence_error(ObjectType, Culprit), Imp_def) :-
  write($existence error: $), write(ObjectType), tab(1), write(Culprit), 
  write($ does not exist $), nl.
isoError(permission_error(Operation, ObjectType, Culprit), Imp_def) :-
  write($permission error: $), write(Culprit), 
  write($ does not have permission to $), write(Operation), write($ on $), 
  write(ObjectType), nl.
isoError(representation_error(Flag), Imp_def) :-
  write($representation error: $), write(Flag), nl.
isoError(calculation_error(Error), Imp_def) :-
  write($calculation error: $), write(Error), nl.
isoError(resource_error(Resource), Imp_def) :-
  write($resource error: $), write(Resource), nl.
isoError(instantiation_error, Imp_def) :-
  write($instantiation error\n$).
isoError(syntax_error, Imp_def) :-
  write($syntax error: $), nl.
isoError(system_error, Imp_def) :-
  write($system error: $), nl.

% files are the files on the command line or read in, .pro, .plm, .pal
% options are the letters of each of the options
%doit([S|_], Options) :- 
                              %(member(p, Options) ; member('P', Options)), !,
                                              %proof(S).

doit([S, O, L], Options) :-                   %(retractall(wamout(_)) ; true),
  % (retractall(wamout(_)) ;  true),
  abolish(wamout/1),
  set_options([S, O, L], [S2, O2, L2], Options),
  (L2 = null, retractall(list_on) ;  assert(list_on)), !, % flow(compile),
  (debug64_compile ->
     nl, write('*** debugging ***'), nl, nl,
     debug_compile(S2, O2, L2)
     ;
     compile(S2, O2, L2) ).

set_options(Fs, Fs, []).
set_options(Fs, FFs, [O|Os]) :-

% write($setting option -$ : O), nl, 
  set_opt(Fs, F2s, O),
  set_options(F2s, FFs, Os).

set_opt(X, X, d) :-
  assert(debug64_compile).
set_opt([S|X], [S|X], a) :-
  add_wamout(S).
set_opt([S|X], [S|X], 'A') :-
  add_wamout(S).
set_opt([S, O, user], [S, O, null], nol) :- !.
set_opt(Fs, Fs, _).

add_wamout(S) :-
  atom_codes(S, Sx),
  file_name_ext(Sx, Rootx, _),
  append(Rootx, ".wam", Namex),
  atom_codes(N, Namex),
  assert(wamout(N)).

user_inputs(Files, Options) :-
  get_command_line(C),                        % list of lists
  opts_n_files(Options, Fs, C, []),
  just_help(Options),
  (Fs = [], prompt_for_files(Fils) ;  Fils = Fs),
  apply_defaults(Fils, Files), !.

get_command_line(L) :-
  command_line(L), !.
get_command_line([]).

% note there is a problem with 'char' and 'int'
% notations for characters, don't always line up
% right, so this name('-', DASH) is a kludge
% to fix it for now. Same with DOT below.

opts_n_files([O|Os], Fs) -->
  {atom_codes('-', [DASH])},

% ([[0'-|X]] ; [[0'/|X]]),
  [[DASH|X]],
  {atom_codes(O, X)},
  opts_n_files(Os, Fs).
opts_n_files(Os, [F|Fs]) -->
  [X],
  {filename(F, X, [])},                       % cons file list
  opts_n_files(Os, Fs).
opts_n_files([], []) --> [].

filename(file(Name, Ext), FullName, L) :-
  reverse(FullName, FullRName),
  filername(file(RName, RExt), FullRName, L),
  (nonvar(RName), reverse(RName, Name) ;  true),
  string_list(S2, Name),
  (nonvar(RExt) -> reverse(RExt, Ext) ;  true).
filename(_, X, _) :-
  string_list(S, X),
  throw(comperr(badfilename, S)).
filename(_, X, _) :-
  throw(comperr(badfilename, X)).

% note there is a problem with 'char' and 'int'
% notations for characters, don't always line up
% right, so this name('.', DOT) is a kludge
% to fix it for now. Same with DASH above.

filername(file(Name, Ext)) -->
  extchars(Ext),
  {atom_codes('.', [DOT])},
  [DOT],
  chars(Name),
  {listlen(Ext, L), L =< 3}.
filername(file(Name, _)) -->
  chars(Name).
filername(file(_, _)) --> [].

extchars([C|Cs]) -->
  extchar(C),
  extchars(Cs).
extchars([C]) -->
  extchar(C).

extchar(C) -->
  [C],
  {C >= 0'a, C =< 0'z}.
extchar(C) -->
  [C],
  {C >= 0'A, C =< 0'Z}.
extchar(C) -->
  [C],
  {C >= 0'0, C =< 0'9}.

chars([C|Cs]) -->
  char(C),
  chars(Cs).
chars([C]) -->
  char(C).

%char(C) --> [C], {C >= 0'a, C =< 0'z}.
%char(C) --> [C], {C >= 0'A, C =< 0'Z}.
%char(C) --> [C], {C >= 0'0, C =< 0'9}.
% These are the long file name rules for NT, all characters are OK except
% for the ones listed here. / \ : are also illegal in file names but are
% legal in directory names so they are allowed here.

char(C) -->
  [C],
  {member(C, [0'?, 0'", 0'<, 0'>, 0'*, 0'|]), !, fail}.
char(C) -->
  [C].

just_help([]).
just_help(['?'|Os]) :-
  about_compiler.
just_help([_|Os]) :-
  just_help(Os).

about_compiler :-
  write($Usage: $), 
  write($a5cmp <-a> <filename><.PRO> <filename><.PLM> <filename><.PAL> \n$), 
  write($  .PRO is the source file\n$), 
  write($  .PLM is the generated object file\n$), 
  write($  .PAL is an optional file echoing the compiler messages\n$), 
  write($  only the filename need be given, e.g. a5cmp foo\n$), 
  write($  if no arguments are given, acmp will prompt for them\n$), 
  write($\n$), 
  write($  option -a generates listing of WAM codes in <filename>.WAM \n$), 
  halt.

prompt_for_files([file(SrcName, SrcExt), Object, Listing]) :-
  write($\nSource Code [.PRO]: $), 
  read_fname(file(SrcName, SrcExt)),
  (var(SrcName) -> throw(comperr(badfilename, SrcName)) ;  true),
  atom_codes(SourceName, SrcName),
  write($Object Code [$), write(SourceName), write($.PLM]: $), 
  read_fname(Object),
  write($Listing File [NULL.PAL]: $), 
  read_fname(Listing).

read_fname(F) :-
  read_string(S),
  (S == $$ -> F = file(_, _) ;  string_list(S, L), filename(F, L, [])).

apply_defaults(Fils, [Source, Object, Listing]) :-
  fillout(Fils, FO),
  FO = [file(S, Sx), file(O, Ox), file(L, Lx)],
  (var(Sx), Sx = "pro" ;  true),
  (var(O), O = S ;  true),
  (var(Ox), Ox = "plm" ;  true),
  concat_lists([S, ".", Sx], Src),
  atom_codes(Source, Src),
  concat_lists([O, ".", Ox], Obj),
  atom_codes(Object, Obj),
  (
     var(L),
     Listing = user ;

     (var(Lx), Lx = "pal" ;  true),
     concat_lists([L, ".", Lx], Lst),
     atom_codes(Listing, Lst)
  ), !.

fillout([S], [S, file(_, _), file(_, _)]).
fillout([S, O], [S, O, file(_, _)]).
fillout(X, X).

concat_lists([One, Two], Result) :-
  append(One, Two, Result).
concat_lists([One, Two|Rest], Result) :-
  append(One, Two, X),
  concat_lists([X|Rest], Result).

pip(L, Pre, Inf, Post) :-
  append(L1, Post, L),
  append(Pre, Inf, L1).

file_name_ext(S, Pre, Post) :-
  pip(S, Pre, ".", Post), !.
file_name_ext(S, S, []).

listlen(L, N) :-
  llen(L, 0, N).

llen([], N, N).
llen([A|Y], X, N) :-
  XX is X + 1,
  llen(Y, XX, N).

% report/1 is used in other compiler modules so that there
% isn't a write/1 compiled, so that it can be used in embedded
% applications, such as from VB.

report(X) :-
  write(X).

:- end_body(amzi_compiler).

