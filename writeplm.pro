%-*-Prolog-*-  
% writeplm indented on 11/29/2001 by 'JOLI' 1.0.

%-----------------------------------------------------------------------
%
% writeplm.pro -- write out APAL code to text
%
% Copyright (c) 1992-2002 Amzi! inc. All Rights Reserved.
%
% $Log: writeplm.pro,v $
% Revision 1.1.1.1  2003/09/11 02:15:10  dennis
% Starting release 7.0
%
% Revision 1.6  2002/05/15 16:59:07  dennis
% Final fixes for last 6.1 build, 80
%
% Revision 1.5  2002/04/25 03:42:22  dennis
% more documentation, logicbase.htm, and some fiddling with sources
%
% Revision 1.4  2002/01/20 20:48:05  ray
% revised real divide, printReal
%
% Revision 1.3 2001/02/08 22:56:45 dennis
% string bug fixes, modularized compiler and listener
%
% Revision 1.2 2001/01/30 16:47:28 dennis
% Made, after many trials, alib into amzi_system module.
%
% Revision 1.1.1.1 2000/12/29 02:18:05 dennis
% moved to a6
%
% Revision 1.5 2000/09/25 02:11:19 dennis
% first version of modules working, runs the modular version of
% duck world. still needs import and export. release 6.1.1
%
% Revision 1.4 2000/03/28 01:05:14 dennis
% merged Ray's changes with bigdig. bigdig is at point where
% new Cell class is used, but there are no modules in the system.
%
% Revision 1.3.2.1 2000/02/26 20:56:13 dennis
% Removed local atoms from compiler, and old module support, so
% compiler and listener are all global for now. Also made member/2
% and friends built-ins as well as the bug predicates.
%
%
% ::Source Control Info:: "%v / %f"
% Version/Date "2 / 16-Mar-93"
%
% ::Revision History::
% 1 WRITEPLM.PRO 9-Nov-92 "Version 2.0 32-bit Alpha"
% 2 WRITEPLM.PRO 16-Mar-93 add write plm support
% ::Revision History::
%
%-----------------------------------------------------------------------

% bigdig :- export write_plm/2.

:- body(amzi_compiler).

write_plm(Name/Arity, List) :-
  write('procedure   '), write(Name/Arity), nl, nl, 
  xwrite_plm(List),
  fail.
write_plm(_, _) :-
  nl.

xwrite_plm(List) :-
  member(I, List),
  welem(I),
  fail.
xwrite_plm([]) :-
  nl, !.

wcomma([A]) :-
  warg(A),
  nl.
wcomma([A|L]) :-
  warg(A),
  write(','), 
  wcomma(L).
wcomma([]) :-
  nl.

sp :-
  write('  ').

und :-
  write('_').

wtab(X) :-
  tab(8), write(X).

wln(X) :-
  write(X), nl.

wtabln(X) :-
  tab(8), write(X), nl.

warg(A) :-
  (
     nonvar(A),
     A = x(I),
     write('X'), write(I) ;

     nonvar(A),
     A = y(I),
     write('Y'), write(I) ;

     number(A),
     write('&'), write(A) ;

     write(A)
  ).

welem(X) :-
  atomic(X),
  wtabln(X), !.
welem(allocate(V)) :-
  wtab(allocate),
  sp,
  wln(V), !.
welem(fail/0) :-
  wtabln(fail), !.
welem(label(L)) :-
  write(L), 
  wln(':'), !.
welem(pair(A, B)) :-
  wtab(A),
  put(9), 
  wln(B), !.
welem(Instr) :-
  Instr =.. [switch_on_term|Args],
  wtab(switch_on_term),
  sp,
  wcomma(Args), !.
welem(switch(Kind, Len, Fudge)) :-
  wtab(switch_on_),
  write(Kind), 
  sp,
  write(Len), write(','), 
  wln(Fudge), !.
welem(unify(void, N)) :-
  wtab(unify_void),
  sp,
  wln(N), !.
/*
**   unify_x, unify_y instructions 
*/
welem(unify(Type, Arg)) :-
  (Type == value ;  Type = variable),
  Arg =.. [Reg, I],
  wtab(unify),
  und,
  write(Reg), 
  und,
  write(Type), 
  sp,
  warg(Arg),
  nl, !.
/*
** put_x_  put_y_  instructions
*/
welem(put(Type, Ri, Xi)) :-
  (Type == value ;  Type == variable),
  Ri =.. [Reg, I],
  wtab(put),
  und,
  write(Reg), 
  und,
  write(Type), 
  sp,
  wcomma([Ri, Xi]), !.
/*
** get_x_  get_y_  instructions
*/
welem(get(Type, Ri, Xi)) :-
  (Type == variable ;  Type == value), !,
  Ri =.. [Reg, I],
  wtab(get),
  und,
  write(Reg), 
  und,
  write(Type), 
  sp,
  wcomma([Ri, Xi]), !.
/*
**  generic register op_codes
*/
welem(Instr) :-
  Instr =.. [Name, Type|Args],
  (Name = unify ;  Name = get ;  Name = put),
  wtab(Name),
  und,
  write(Type), 
  sp,
  wcomma(Args), !.
welem(try(NTV, Label)) :-
  wtab(try),
  sp,
  write(Label), write(' / '), 
  wln(NTV), !.
welem(tryor(NTV, else, Label)) :-
  wtab('try_me_or_else'),
  sp,
  write(Label), write(' / '), 
  wln(NTV), !.
welem(Instr) :-
  Instr =.. [Name, Arg1|Args],
  (Name = retry ;  Name = trust),
  wtab(Name),
  (
     Arg1 == else ->
     write('_me_else  '), 
     Args = [L],
     wln(L) ;

     sp,
     wln(Arg1)
  ), !.
welem(Instr) :-
  Instr =.. [Name, Arg],
  (Name = get_nil ;  Name = put_nil ;  Name = get_list ;  Name = put_list),
  wtab(Name),
  sp,
  warg(Arg),
  nl, !.
welem(Instr) :-
  Instr =.. [Name, Arg],
  wtab(Name),
  sp,
  wln(Arg), !.
welem(call(Name, N)) :-
  wtab(call),
  sp,
  write(Name), write(','), 
  wln(N), !.
welem(mod_call(Mod, Name, N)) :-
  wtab(mod_call), sp, write(Mod : Name), write(','), wln(N), !.
welem(mod_execute(Mod, Name)) :-
  wtab(mod_execute),
  sp,
  write(Mod : Name), nl, !.
welem(Name/Arity) :-
  wtab(escape),
  sp,
  wln(Name/Arity), !.

w(Expr) :-
  X is Expr,
  write(Expr).

wl([A|Rest]) :-
  write(A), nl, 
  wl(Rest).
wl([]).

:- end_body(amzi_compiler).


