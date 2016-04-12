%-*-Prolog-*-  
% cclause indented on 12/23/2001 by 'JOLI' 1.0.

%------------------------------------------------------------------------
%
% cclause.pro -- compile a clause
%
% Copyright (c) 1992-2002 by Amzi! inc. All Rights Reserved.
%
% $Log: cclause.pro,v $
% Revision 1.2  2004/05/05 16:31:44  dennis
% added tcltk stuff
%
% Revision 1.1.1.1  2003/09/11 02:15:10  dennis
% Starting release 7.0
%
% Revision 1.15  2003/08/23 03:27:31  dennis
% cut working for debug64 compiled code
%
% Revision 1.14  2002/09/27 00:27:23  dennis
% fix another retract pulls the rug out from goal bug
%
% Revision 1.13  2002/05/15 16:59:06  dennis
% Final fixes for last 6.1 build, 80
%
% Revision 1.12  2002/04/25 03:42:22  dennis
% more documentation, logicbase.htm, and some fiddling with sources
%
% Revision 1.11  2002/04/19 19:41:42  dennis
% fixed retract bug with sorted/indexed clauses, implemented abolish for
% those types as well
%
% Revision 1.10  2002/03/19 00:30:30  dennis
% add infinite compiles by breaking up predicates every 1000 clauses,
% used discontiguous feature, so no problem with huge numbers of clauses.
%
% Revision 1.9  2002/02/21 21:08:31  dennis
% changed to floats/single for number defaults
%
% Revision 1.8  2002/01/20 20:48:05  ray
% revised real divide, printReal
%
% Revision 1.7 2001/10/19 01:37:59 dennis
% compiler bugs, still not found, but noted that X registers
% are really restricted to 255 because of flewrite in assemb.pro,
% should change some day.
%
% Revision 1.6 2001/09/11 04:34:55 dennis
% cleaned up some io stuff, got consult working, etc.
%
% Revision 1.5 2001/06/27 15:15:09 dennis
% miscellaneous changes and bug fixes, work with leak detection
%
% Revision 1.4 2001/03/16 00:29:06 dennis
% compiled metapredicates
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
% Revision 1.9 2000/11/30 15:47:27 ray
% made arg, nth and prime bilateral.
% made real length and exponent 12 bits for maximum range
%
% Revision 1.8 2000/10/07 17:47:19 ray
% added q_cf and fourierPrime
%
% Revision 1.7 2000/09/25 02:11:18 dennis
% first version of modules working, runs the modular version of
% duck world. still needs import and export. release 6.1.1
%
% Revision 1.6 2000/09/15 21:42:24 dennis
% 12->13
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
% Revision 1.3.2.1 2000/02/26 20:56:12 dennis
% Removed local atoms from compiler, and old module support, so
% compiler and listener are all global for now. Also made member/2
% and friends built-ins as well as the bug predicates.
%
%
% 09/13/99 Ray rewrote conflict to not use is_member (for)
%
% 1 CCLAUSE.PRO 9-Nov-92 "Version 2.0 32-bit Alpha"
% $Log: cclause.pro,v $
% Revision 1.2  2004/05/05 16:31:44  dennis
% added tcltk stuff
%
% Revision 1.1.1.1  2003/09/11 02:15:10  dennis
% Starting release 7.0
%
% Revision 1.15  2003/08/23 03:27:31  dennis
% cut working for debug64 compiled code
%
% Revision 1.14  2002/09/27 00:27:23  dennis
% fix another retract pulls the rug out from goal bug
%
% Revision 1.13  2002/05/15 16:59:06  dennis
% Final fixes for last 6.1 build, 80
%
% Revision 1.12  2002/04/25 03:42:22  dennis
% more documentation, logicbase.htm, and some fiddling with sources
%
% Revision 1.11  2002/04/19 19:41:42  dennis
% fixed retract bug with sorted/indexed clauses, implemented abolish for
% those types as well
%
% Revision 1.10  2002/03/19 00:30:30  dennis
% add infinite compiles by breaking up predicates every 1000 clauses,
% used discontiguous feature, so no problem with huge numbers of clauses.
%
% Revision 1.9  2002/02/21 21:08:31  dennis
% changed to floats/single for number defaults
%
% Revision 1.8  2002/01/20 20:48:05  ray
% revised real divide, printReal
%
% Revision 1.7 2001/10/19 01:37:59 dennis
% compiler bugs, still not found, but noted that X registers
% are really restricted to 255 because of flewrite in assemb.pro,
% should change some day.
%
% Revision 1.6 2001/09/11 04:34:55 dennis
% cleaned up some io stuff, got consult working, etc.
%
% Revision 1.5 2001/06/27 15:15:09 dennis
% miscellaneous changes and bug fixes, work with leak detection
%
% Revision 1.4 2001/03/16 00:29:06 dennis
% compiled metapredicates
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
% Revision 1.9 2000/11/30 15:47:27 ray
% made arg, nth and prime bilateral.
% made real length and exponent 12 bits for maximum range
%
% Revision 1.8 2000/10/07 17:47:19 ray
% added q_cf and fourierPrime
%
% Revision 1.7 2000/09/25 02:11:18 dennis
% first version of modules working, runs the modular version of
% duck world. still needs import and export. release 6.1.1
%
% Revision 1.6 2000/09/15 21:42:24 dennis
% 12->13
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
% Revision 1.3.2.1 2000/02/26 20:56:12 dennis
% Removed local atoms from compiler, and old module support, so
% compiler and listener are all global for now. Also made member/2
% and friends built-ins as well as the bug predicates.
%
% Revision 1.3 2000/01/20 10:25:41 dennis
% Put in Ray's 1999-11-11 release, which included the fix for
% large predicates, which also requires 32-bit alignment. r5-0-2
%
% Revision 4.3 1998/01/16 04:14:09 dennis
% RCS Test2 - It works pretty good!
%
% Revision 4.2 1998/01/16 04:11:24 dennis
% RCS test
%
% Revision 4.1 1998/01/16 04:03:26 dennis
% Fixed bug compiling -> ; structures caused by change in precedence of
% the -> operator to comply with ISO standard.
%
%------------------------------------------------------------------------
/************************************************************************
*     compile one clause : use several passes writing out
*     intermediate files and then failing to collect global stack !!
************************************************************************/

/***********************************************************************
*               Version 1.1 -- modularized the code
*               Version 1.1A 
************************************************************************/
:- body(amzi_compiler).

%% Consider 3 cases for Clause ..
%% Head
%% Head :- OneGoal
%% then we quick compile it - no need for perm vars, and all the code
%% (should) fit in one pass
%%
%% Head :- Body 
%% Run the three passes. Use Dis (bound to dis if a disjunction in the 
%% body) to optionally run initvars() if there is a disjunction in
%% the body

get_head((H :- Body), H) :- !.
get_head(H, H).

compileclause(Clause, Code) :-
  pretrans(Clause, Pretrans),                 %!,
  compc(Pretrans, Code), !.

compc((Head :- Body), Code) :-
  functor(Body, Func, _),
  Func \= ',',
  Func \= ';', !,
  quickcomp((Head :- Body), Code).
compc((Head :- Body), Code) :- !,

                         % this is where the various passes talk to each other
  reserve$(Heap),
  repeat,
  (pass1(Heap, (Head :- Body)) ;  pass2(Heap, Code) ;  pass3(Heap, Code)), !.
compc(Clause, Code) :-
  quickcomp(Clause, Code).

                       % Ram everything into one pass - no permanent variables

quickcomp(Clause, Code) :-
  get_head(Clause, Head),
  functor(Head, N, A),
  cntr_get(1, ClauseN), write_l(ClauseN),
  write_l($ |-- $),                            % keep user informed
  write_l(N/A),
  write_l($\n$),
  unravel(Clause, Unravel, _),
  partobj(Unravel, Partobj, []),
  valvar(Partobj, Head, []),

% ? time_it(varlist(Unravel, VarList)), no need to time?
  varlist(Unravel, VarList),
  lifetime(VarList, LifeList, _, _),
  tempalloc(VarList, LifeList),
  objcode(Partobj, Objcode),
  peephole(Objcode, Code), !.

pass1(H, Clause) :-
  get_head(Clause, Head),                     % src->src translations
  functor(Head, N, A),
  cntr_get(1, ClauseN), write_l(ClauseN),
  write_l($ |-- $),                            % keep user informed
  write_l(N/A),
  write_l($\n$),
  permvars(Clause, Perms),                    % list of Yi s 
  unravel(Clause, Unravel, Disjunction),
  partobj(Unravel, Partobj, Perms),
  valvar(Partobj, Head, Perms),
  permalloc(Perms),

% time_it(varlist(Unravel, Varlist)),
  varlist(Unravel, Varlist),
  stash$(H, pass$(Varlist, Partobj, Disjunction)), !,
  fail.

pass2(H, Code) :-
  get$(H, [pass$(VarList, Partobj, Dis)]),
  lifetime(VarList, LifeList, Forward, Backward),
  (
     Dis == dis ->                            % Go onto third pass
     varinit(Forward, Backward, Partobj, Newobj),
     tempalloc(VarList, LifeList),
     stash$(H, pass$(Newobj)),
     fail ;                                  % Else can fit it all in 2nd pass

     tempalloc(VarList, LifeList),
     objcode(Partobj, Objcode),
     peephole(Objcode, Code)
  ), !.

pass3(H, Code) :-
  get$(H, [pass$(Newobj)]),
  objcode(Newobj, Objcode),
  peephole(Objcode, Code), !.

/************************************************************************
*   pretransformations - source->source transformations
************************************************************************/

pretrans((Head :- Body), (Head :- PB)) :- !,
  xpretrans(Body, PB).
pretrans(Head, Head).

% the case where a variable is a goal, meant to be
% called when instantiated, poor style I think,
% an explicit call is better in the user's code,
% but what the customer wants.
xpretrans(A, call(A)) :-
  var(A), !.

/* Conditional */

/* It turns out that the cut used here should work the way
   Allan intended the cutd to work, which is incorrect for
   disjunctions, but gives the desired (but maybe not correct?)
   behavior of A->B;C constructs.  So, we preserve the cutd
   for this case.
   */

/* Well, its even worse.  If the user puts cuts within any of the
   goals, A, B, or C, then those cuts should be opaque, that is, they
   shouldn't affect the predicate they are in.  (As opposed to cuts
   within disjunctions, which are transparent, affecting the predicate
   they are in.)  So, rather than keep these optimizations, it's
   better to simply use the builtins in amzilib.pro, which define these
   as separate predicates, so it all works ok. */

/* And still worse. If we use amzilib.pro, then goals that are local
   predicates aren't seen, and fail.  So not(foo) doesn't work if foo
   is a local predicate.  Argghhh.  What are the fixes?  Make a cutdd or
   something that behaves correctly when cuts are included in -> ; constructs.
   They must, of course, be different than real disjunctions which use
   transparent cuts.  The other fix is to get rid of local predicates
   and modules altoghether, as they're not standard anyway.  Not sure
   I like that either. */

/* In 4.01 we changed the priority of -> to be (1050) less than
   the 1100 of ;.  It used to be 1150, so it was necessary to add
   the extra parentheses around A->B to correct an awful mess. */

%/*
xpretrans(((A -> B) ;  C), (If_Then ;  Else)) :-
  cut_check(A),
  cut_check(B),
  cut_check(C),
  xpretrans(A, PA),
  xpretrans(B, PB),

% conj_append(PA, (!, PB), If_Then),
  conj_append(PA, (cut$d, PB), If_Then),      % cutbug fix
  xpretrans(C, Else), !.
xpretrans((A -> B), (If_Then ;  fail)) :-
  cut_check(A),
  cut_check(B),
  xpretrans(A, PA),
  xpretrans(B, PB),

% conj_append(PA, (!, PB), If_Then),
  conj_append(PA, (cut$d, PB), If_Then), !.   % cutbug fix
%*/
                                 % simple arithmetic (+ or - constant, X + Y) 
xpretrans((X is Y), (X is Y)) :-
  var(Y), !.

/*
xpretrans((X is A - Const), arith_plus(X, A, NegConst)) :-
   integer(Const),
   NegConst is - Const, !.
xpretrans((X is A + B), arith_plus(X, A, B)) :- !.
*/

/* These too must be replaced with the kludge cut$d
xpretrans(not(A), (NA ; true)) :-
        xpretrans(A, PA),
        conj_append(PA, (!, fail), NA),
        !.
xpretrans(\+(A), (NA ; true)) :-
        xpretrans(A, PA),
        conj_append(PA, (!, fail), NA),
        !.
*/

/* experimentally, lets take these out and let the definitions
   in amzilib.pro take over --- experiment a success, these
   optimizations will remain out. Nope, looks like back in.*/
xpretrans(not(A), (NA ;  true)) :-
  cut_check(A),
  xpretrans(A, PA),
  conj_append(PA, (cut$d, fail), NA), !.
xpretrans(\+(A), (NA ;  true)) :-
  cut_check(A),
  xpretrans(A, PA),
  conj_append(PA, (cut$d, fail), NA), !.
xpretrans((Goal, Body), PGB) :-
  xpretrans(Goal, PG),
  xpretrans(Body, PB),
  conj_append(PG, PB, PGB), !.
xpretrans((Choice ;  Others), (PC ;  PO)) :-
  xpretrans(Choice, PC),
  xpretrans(Others, PO), !.

%xpretrans( \=(X, Y), (X = Y, !, fail ; true)) :- !.
% this too needs the cut$d fix, but is now not used
xpretrans(\=(X, Y), (X = Y, cut$d, fail ;  true)) :- !.


xpretrans(M : GIN, GOAL) :-
  amzi_system:is$meta(M, GIN, DM), !,
  (
     amzi_system:mcheck$convert(M, GIN, GOUT, DM) ->
     GOAL = mod$call(M, GOUT) ;

     GOAL = (meta$convert(M, GIN, GOUT2, DM), call_nometa(M : GOUT2))
  ).

xpretrans(Mod : Pred, mod$call(Mod, Pred)) :- !.

% mcheck does a convert of the arguments, unless it runs into
% unbound variables, in which case it fails. So we either do
% the job now, or put the call in so it gets automatically done
% at runtime.
xpretrans(GIN, GOAL) :-
  loading_module(CM),
  amzi_system:is$meta(CM, GIN, DM),

  (
     amzi_system:mcheck$convert(CM, GIN, GOUT, DM) ->
     GOAL = GOUT ;

     GOAL = (meta$convert(CM, GIN, GOUT2, DM), call_nometa(GOUT2))
  ), !.

%write(trans2out-GOAL),nl,
xpretrans(A, A).                            % join together two goal sequences

conj_append((A, L1), L2, (A, L3)) :-
  conj_append(L1, L2, L3).
conj_append(A, L, (A, L)).

cut_check(!) :-
  cut_warning, !.
cut_check((!, _)) :-
  cut_warning, !.
cut_check((A, B)) :-
  cut_check(B).
cut_check((A ;  B)) :-
  cut_check(A),
  cut_check(B).
cut_check(_).

cut_warning :-
  write_l($Warning: Dangerous cut in 'not' or 'if-then-else'\n$).

/**************************************************************************
***             Identify and number permanent variables          **********
***************************************************************************/

                                              % find all permanent variables

permvars((Head :- Body), Perms) :- !,
  varsof(Head, HeadVars),
  xpermvars(Body, [HeadVars, [], []], [_, _, Perms]), !.
permvars(Head, []).

xpermvars(X, SoFar, Out) :-                   % disjunction
  Dis = (_ ;  _),
  (X = (Dis, Rest) ;  X = Dis), !,
  disxpermvars(Dis, SoFar, NewSoFar),
  (nonvar(Rest) -> xpermvars(Rest, NewSoFar, Out) ;  Out = NewSoFar).
xpermvars(X, SoFar, Out) :-                   % conjunction 
  (X = (A, Rest) ;  X = A),
  SoFar = [Vars, Half, Perms],
  varsof(A, AVars),
  intersectv(AVars, Half, P), !,
  unionv(Perms, P, NewPerms),
  unionv(AVars, Vars, NewVars),
  (builtin(A) -> NewHalf = Half ;  unionv(NewVars, Half, NewHalf)),
  NewSoFar = [NewVars, NewHalf, NewPerms],
  (nonvar(Rest) -> xpermvars(Rest, NewSoFar, Out) ;  Out = NewSoFar).

disxpermvars((A ;  B), SoFar, Out) :- !,
  xpermvars(A, SoFar, OutA),
  disxpermvars(B, SoFar, OutB), !,
  map_unionv(OutA, OutB, Out), !.       % unionv puts newvars at end of Perms 
disxpermvars(B, SoFar, Out) :-
  xpermvars(B, SoFar, Out), !.

permalloc(PermVars) :-    % trivial permalloc, vars at end are numbered lowest
  permalloc(PermVars, _).

permalloc([y(I)|Vars], I) :-
  permalloc(Vars, I1),
  I is I1 + 1.
permalloc([], 0).

/************************************************************************
**  All structures are unravelled into unify goals
**  All unify goals are of the form Var1 = (Var2 or Atom or Struc)
**  Where var1 is temp. or permanent. and where Struc
**  has only variables and atoms as arguments
**  If var1 is permanent so is var2
**  Preexisisting Unify goals are converted to this type
**  ; remains - only the content is unravelled
** If body contains ; then Dis == dis else var(Dis)
************************************************************************/

unravel(X, [NewHead|Ravel], Dis) :-
  (X = (Head :- Body) ;  X = Head),
  top_spread(Head, NewHead, Ravel, L),
  (nonvar(Body) -> xunravel(Body, L, [], Dis) ;  L = []), !.

xunravel(X, [DRavel|Ravel], Link, dis) :-     % dis flags a disjunction
  Dis = (_ ;  _),
  (X = (Dis, Rest) ;  X = Dis), !,
  disunravel(Dis, DRavel), !,
  (nonvar(Rest) -> xunravel(Rest, Ravel, Link, _) ;  Ravel = Link), !.
xunravel(X, Ravel, Link, Dis) :-
  Goal = (_ = _),
  (X = (Goal, Rest) ;  X = Goal), !,
  varunify(Goal, Ravel - L),
  (nonvar(Rest) -> xunravel(Rest, L, Link, Dis) ;  L = Link), !.
xunravel(X, Ravel, Link, Dis) :-
  (X = (Goal, Rest) ;  X = Goal), !,
  top_spread(Goal, NewGoal, Ravel, L), !,
  L = [NewGoal|L2],
  (nonvar(Rest) -> xunravel(Rest, L2, Link, Dis) ;  L2 = Link), !.

disunravel((A ;  B), (ARavel ;  BRavel)) :- !,
  xunravel(A, ARavel, [], _), !,
  disunravel(B, BRavel).
disunravel(A, ARavel) :-
  xunravel(A, ARavel, [], _).

/************************************************************************
**  Unification optimization
**  Turn simple goal X = Y into sequence of simpler unifications of
**  the form Var1 = (Var2 or atom or struc),
**  where Var1 is temp or perm and where struc has only atoms
**  and variables as args
************************************************************************/

varunify(X = Y, Code - Link) :-
  (xvarunify(X = Y, Code, Link) ;  Code = [fail|Link]), !.

xvarunify(A = B, [(A = NewB)|L], Link) :-     % or one or the other is a var 
  var(A), !,
  spread(B, NewB, L, Link).
xvarunify(A = B, [(B = NewA)|L], Link) :-
  var(B), !,
  spread(A, NewA, L, Link).
xvarunify(A = B, Code, Link) :-               % both args are nonvars 
  atomic(A), !,
  atomic(B),
  (A = B -> Code = Link ;  fail).
xvarunify(A = B, Code, Link) :-
  (
     atomic(B) ->
     fail ;                                   % A & B are strucs 

     A =.. [Func|ArgsA],
     B =.. [Func|ArgsB],
     lvarunify(ArgsA, ArgsB, Code, Link)
  ).

lvarunify([A|ArgsA], [B|ArgsB], Code, Link) :-
  xvarunify(A = B, Code, L), !,
  lvarunify(ArgsA, ArgsB, L, Link).
lvarunify([], [], Link, Link).

/************************************************************************
**  Take a (possibly nested) structure apart into 
**       1 a simple structure
**       2 a series of unify goals. 
**  A list is considered as a structure with variable arity. 
*************************************************************************/

top_spread(List, SimpleList, Rest, Link) :-
  list(List), !,
  argspread(CdrUnify, List, SimpleList, Ravel, Link), !,
  (CdrUnify = none -> Rest = Ravel ;  Rest = [CdrUnify|Ravel]).
top_spread(mod$call(Mod, Goal), mod$call(Mod, SimpleStruc), R, L) :-
  functor(Goal, Name, _),
  Goal =.. [_|Args], !,
  argspread(_, Args, VArgs, R, L), !,
  SimpleStruc =.. [Name|VArgs].
top_spread(Struc, SimpleStruc, R, L) :-
  functor(Struc, Name, _),
  Struc =.. [_|Args], !,
  argspread(_, Args, VArgs, R, L), !,
  SimpleStruc =.. [Name|VArgs].
top_spread(Other, Other, Link, Link).

spread(List, SimpleList, Rest, Link) :-
  list(List), !,
  argspread(CdrUnify, List, SimpleList, Ravel, Link), !,
  (CdrUnify = none -> Rest = Ravel ;  Rest = [CdrUnify|Ravel]).
spread(Struc, SimpleStruc, R, L) :-
  functor(Struc, Name, _),
  Struc =.. [_|Args], !,
  argspread(_, Args, VArgs, R, L), !,
  SimpleStruc =.. [Name|VArgs].
spread(Other, Other, Link, Link).

argspread(CdrUnify, Cdr, T, R, L) :-
  nonlist(Cdr), !,
  (
     (var(Cdr) ;  Cdr = []) ->
     CdrUnify = none,
     T = Cdr,
     R = L ;

     spread(Cdr, SimpleCdr, R, L),
     CdrUnify = (T = SimpleCdr)
  ), !.
argspread(CdrUnify, [A|Args], [A|VArgs], R, L) :-
  (atomic(A) ;  var(A)), !,
  argspread(CdrUnify, Args, VArgs, R, L).
argspread(CdrUnify, [S|Args], [T|VArgs], Ravel, Link) :-
  Ravel = [(T = V)|L],
  spread(S, V, L, L2), !,
  argspread(CdrUnify, Args, VArgs, L2, Link).

                              % Convert unravelled code to partial object code

partobj([Head|BodyGoals], [HeadObj|BodyObj], Perms) :-
  Head =.. [_|Args],
  gp_block(get, Args, HeadObj, 1),
  xpartobj(BodyGoals, Perms, BodyObj, yes), !.

xpartobj([], _, [], _) :- !.
xpartobj([Dis|Rest], Perms, Result, Flag) :-
  Dis = (_ ;  _), !,
  (            % Initialize permanent variables just before first disjunction 
     Flag = yes ->
     initblock(Perms, PermInit),
     Result = [PermInit, DisCode|RestCode] ;

     Result = [DisCode|RestCode]
  ),
  dispartobj(Dis, Perms, DisCode), !,
  xpartobj(Rest, Perms, RestCode, no).
xpartobj([Goal|Rest], Perms, [GoalCode|RestCode], Flag) :-
  goalpartobj(Goal, Perms, GoalCode), !,
  xpartobj(Rest, Perms, RestCode, Flag).

dispartobj((A ;  B), Perms, (ACode ;  BCode)) :- !,
  xpartobj(A, Perms, ACode, no), !,
  dispartobj(B, Perms, BCode).
dispartobj(A, Perms, ACode) :-
  xpartobj(A, Perms, ACode, no).

/*
        convert goals to object code
        recognizes !, true, fail, unify goals and calls with simple arguments
*/

goalpartobj(!, _, cut) :- !.
goalpartobj(debug64_cut, _, cut64) :- !.

/* this line added to preserve the cutd needed in A->B;C constructs,
   probably not necessary except for clarity of the fix to the kludge. */
goalpartobj(cut$d, _, cut$d) :- !.            % cutdbug fix
goalpartobj(true, _, Link - Link) :- !.
goalpartobj(V = W, Perms, [put(_, V, Temp)|Code] - Link) :-
  (is_member(V, Perms) -> Temp = x(0 /*temp*/) ;  Temp = V), !,
  get_the_get(W, Code, Temp, Link).           % other goals 
goalpartobj(mod$call(Mod, Goal), _, Code - Link) :-
  functor(Goal, Name, Arity),
  Goal =.. [_|Args],
  gp_block(put, Args, Code - L, 1), !,
  L = [mod_call(Mod, Name/Arity, _)|Link].
goalpartobj(Goal, _, Code - Link) :-
  functor(Goal, Name, Arity),
  Goal =.. [_|Args],
  gp_block(put, Args, Code - L, 1), !,
  (
     builtin(Name, Arity) ->
     L = [(Name/Arity)|Link] ;

     L = [call((Name/Arity), _)|Link]
  ).                                       % figure out which get instruction 

get_the_get(W, Code, Temp, Link) :-
  var(W), !,
  Code = [get(_, W, Temp)|Link].
get_the_get(W, Code, Temp, Link) :-
  atomic(W), !,
  Code = [get(constant, W, Temp)|Link].
get_the_get(W, Code, Temp, Link) :-
  (                   % at this point a list is special structure functor '.' 
     list(W) ->
     W = Args,
     Name/Arity = '.'/2,
     Type = list ;

     functor(W, Name, Arity),
     W =.. [_|Args],
     Type = nonlist
  ),
  Code = [get(structure, Name/Arity, Temp)|L], !,

                                              % and generate unify sequence 
  unifyblock(Type, Args, L - Link).

% variable initialisation - register MACRO_MAXVARS used as holder

initblock([], Link - Link).
initblock([V|Vars], [put(_, V, x(0 /*temp*/))|Rest] - Link) :-
  initblock(Vars, Rest - Link).

% get or put all of head arguments (if type is get or put)

gp_block(Type, [A|Args], [X|Rest] - Link, N) :- !,
  X =.. [Type, T, A, x(N)],
  (atomic(A) -> T = constant ;  true),
  N1 is N + 1, !,
  gp_block(Type, Args, Rest - Link, N1).
gp_block(_, [], Link - Link, _).

                    % block of unify instructions to unify structures or lists

unifyblock(nonlist, [], Link - Link) :- !.
unifyblock(nonlist, [A|Args], [unify(T, A)|Rest] - Link) :-
  (atomic(A) -> T = constant ;  true), !,
  unifyblock(Type, Args, Rest - Link).
unifyblock(list, V, Rest - Link) :-
  (
     (atomic(V), Rest = [unify(constant, V)|Link]) ; % cdr 

     (var(V), Rest = [unify(T, V)|Link]) ;    % cdr 

     (                                        % cons 
        V = [A|Args],
        unifyblock(list, A, Rest - L2),       % do cons 
        (                                     % cdr a list
           list(Args) ->
           L2 = 
            [unify(variable, x(0 /*temp*/)), 
            get(structure, '.'/2, x(0 /*temp*/))|L3] ;

           L2 = L3                            % no 
        ), !,
        unifyblock(list, Args, L3 - Link)
     ) ;

     Rest = [unify(T, V)|Link]
  ), !.

/************************************************************************/

/*
        Add initialisation instructions in disjunctions to variables
        which need it. Results in modified partobj.
    
        Must be used before tempalloc
    */

varinit(Forward, Backward, Partobj, Newobj) :-
  xvarinit(Forward, Backward, Partobj, Newobj - []), !.

xvarinit([_], _, X, R - L) :-
  linkify(X, R - L), !.

      % 1st 2 clauses traverse Forward, backward and PartObj till disjunction 
xvarinit([_, FIn|Forward], [_, BIn|Backward], PartObj, NewObj) :-
  not(FIn = (_ ;  _)), !,

 % since Forward and Backward have identical structure only one must be tested
  xvarinit([FIn|Forward], [BIn|Backward], PartObj, NewObj), !.
xvarinit(Forward, Backward, [G|PartObj], [G|NewObj] - Link) :-
  not(G = (_ ;  _)), !,
  xvarinit(Forward, Backward, PartObj, NewObj - Link), !.

                            % at this stage all threee args have disjunctions 
xvarinit([FLeft, (FA ;  FB), FRight|Forward], 
        [_, (BA ;  BB), BRight|Backward], [(A ;  B)|PartObj], 
        [(NA ;  NB)|NewObj] - Link) :- !,
  diffv(FRight, FLeft, T),
  intersectv(T, BRight, V),
  dis_varinit(V, (FA ;  FB), (BA ;  BB), (A ;  B), (NA ;  NB)), !,
  xvarinit([FRight|Forward], [BRight|Backward], PartObj, NewObj - Link).

dis_varinit(V, (FA ;  FB), (BA ;  BB), (A ;  B), (NA ;  NB)) :- !,
  one_choice(V, FA, BA, A, NA), !,
  dis_varinit(V, FB, BB, B, NB).
dis_varinit(V, FA, BA, A, NA) :-
  one_choice(V, FA, BA, A, NA).

one_choice(V, FA, BA, A, NA) :-
  xvarinit(FA, BA, A, NA - Link),
  last(FA, FLast),
  diffv(V, FLast, InitVars),
  (
     InitVars = [] ->
     Link = [] ;

     init_list(InitVars, InitInstr),
     Link = [InitInstr]
  ), !.

init_list([V|Vars], [put(variable, V, V)|Rest] - Link) :-
  init_list(Vars, Rest - Link).
init_list([], Link - Link).

/************************************************************************/

/*
 Turn partial obj code which still contains a hierarchy of goals
 and disjunctions into a uniform list.
 The control instructions for disjunctions are compiled and
 the labels for the cut are instantiated
*/

objcode(PartObj, ObjCode) :-
  xobjcode(PartObj, ObjCode, [], proc, _), !.

/* cutd, which was only used inside of disjunctions,
   was the cause of more than one bug.  First, two
   cutd's in a row to the same label would cause a
   GPF has the engine searched in vain for the label
   after the first cutd had already removed it from
   the choice point stack.

   Second, cutd's in nested or's only cut up to the
   choice points on the disjunction in question, failing
   to freeze choices all the way back to the outer-most
   or.

   What is disturbing is, the fix seems to be just to
   use regular cuts in all places.  Am I missing
   something here?  Why was cutd implemented for 
   disjunctions in the first place?  What problem did
   it solve?  We only know the problems it created.

   Ahh, at least one problem is A->B;C.  It puts in a
   cut which is supposed to behave (although we haven't
   confirmed this with the ISO standard, it is how the
   Cogent version of this is designed) just as the cutd
   is implemented.
   */

xobjcode([], Link, Link, _, _).

/* this line was changed as well, seeing as it's only the
   disjunction guy who cares, and only cutd needs the label */

%xobjcode( [cut|RestCode], Code, Link, CutLbl, yes) :-
/* distinguish between two kinds of cut */

/* Why not make them all regular cuts? */

/*
(       
    CutLbl == proc -> 
    Code = [cut|C] ;
    
    Code = [cutd(CutLbl) | C]
),
   */
xobjcode([cut|RestCode], Code, Link, CutLbl, _) :-
  Code = [cut|C], !,
% write($cutting\n$),
% (CutLbl == proc -> true;
% write($***warning: disjunctive cut ***$),write(CutLbl),nl),
  xobjcode(RestCode, C, Link, CutLbl, _).

xobjcode([cut64|RestCode], Code, Link, CutLbl, _) :-
  Code = [cut64|C], !,
  xobjcode(RestCode, C, Link, CutLbl, _).

/* this clause added for the cut$d case for A->B;C cuts
   which are supposed to behave like cutds. */
xobjcode([cut$d|RestCode], Code, Link, CutLbl, yes) :-
  ( % distinguish between 2 kinds of cut. In this case we want to distinguish 
     CutLbl == proc ->
     Code = [cut|C]
     ;
     Code = [cutd(CutLbl)|C]
  ), !,

% write($d_cutting\n$),
% (CutLbl == proc -> true;
% write($***warning: disjunctive d_cut ***$),write(CutLbl),nl),
  xobjcode(RestCode, C, Link, CutLbl, _).
xobjcode([(Code - L)|RestCode], Code, Link, CutLbl, IsCut) :- !,
  xobjcode(RestCode, L, Link, CutLbl, IsCut).
xobjcode([(X ;  Choices)|RestCode], [tryor(NTV, else, L1)|ChCode], Link, 
        CutLbl, IsCut) :-
  xobjcode(X, ChCode, ChLink, L1, _),
  ChLink = [goto(EndLbl), label(L1)|C3],
  xdiscode(Choices, C3 - L, EndLbl), !,
  xobjcode(RestCode, L, Link, CutLbl, IsCut).

xdiscode((X ;  Choices), [retry(else, L2)|ChCode] - Link, EndLbl) :-
  xobjcode(X, ChCode, ChLink, L2, _),
  ChLink = [goto(EndLbl), label(L2)|C3], !,
  xdiscode(Choices, C3 - Link, EndLbl).
xdiscode(LastChoice, Code - Link, EndLbl) :-
  xobjcode(LastChoice, ChCode, ChLink, CutLbl, IsCut),
  (
     IsCut == yes ->
     Code = [retry(else, CutLbl)|ChCode],
     ChLink = [goto(EndLbl), label(CutLbl), trust(else, fail), (fail/0)|L] ;

     Code = [trust(else, fail)|ChCode],
     ChLink = L
  ), !,
  L = [label(EndLbl)|Link].

/*
         Value-variable annotation
         Assumes that initialisation of variables that needed it have been
         added to the code
     
         Pass1: First occurence of all variables are marked 'variable'.
                All unsafe variables (permanent variables first occurring in
                put) are collected.
     
         Pass2: Do a reverse pass. First encounters of unsafe variables 
                are marked 'unsafe_value' unless they are already marked 
                variable. All other encounters with
                such variables are marked 'value'
     
     
         Must be done before temp allocation and after calculation
         of permanent variables
     
         Vars encountered so far are kept in the set SoFar in both passes. 
         This set is passed in parallel across disjunctions and the different 
         SoFars  are united upon exiting disjunction
     */

                                              % Top level 

valvar(PartObj, Head, Perms) :-
  varsof(Head, HeadVars),
  diffv(Perms, HeadVars, PossUnsafe),
  valvar1(PartObj, _, PossUnsafe, [], UnSafe, [], _), !,
  valvar2(PartObj, _, UnSafe, [], _).         % Pass 1 

valvar1(V, _, _, UnSafe, UnSafe, SF, SF) :-
  var(V), !.
valvar1([], _, _, UnSafe, UnSafe, SF, SF) :- !.
valvar1([H|RestCode], _, PossUS, InUS, OutUS, SoFar, OutSF) :-
  valvar1x(H, RestCode, PossUS, InUS, OutUS, SoFar, OutSF).

valvar1x((A ;  B), RestCode, PossUS, InUS, OutUS, SoFar, OutSF) :-
  disvalvar1((A ;  B), PossUS, InUS, US1, SoFar, NewSF), !,
  valvar1(RestCode, _, PossUS, US1, OutUS, NewSF, OutSF).
valvar1x((G - _), RestCode, PossUS, InUS, OutUS, SoFar, OutSF) :-
  valvar1(G, _, PossUS, InUS, US1, SoFar, NewSF), !,
  valvar1(RestCode, _, PossUS, US1, OutUS, NewSF, OutSF).
valvar1x(unify(T, X), RestInstr, PossUS, InUS, OutUS, SoFar, OutSF) :-
  (
     notin(X, SoFar) ->
     NewSF = [X|SoFar],
     (T = variable ;  true) ;

     NewSF = SoFar
  ), !,
  valvar1(RestInstr, _, PossUS, InUS, OutUS, NewSF, OutSF).
valvar1x(put(T, X, _), RestInstr, PossUS, InUS, OutUS, SoFar, OutSF) :-
  (
     notin(X, SoFar) ->
     NewSF = [X|SoFar],
     (T = variable ;  true),
     (is_member(X, PossUS) -> US1 = [X|InUS] ;  US1 = InUS) ;

     (NewSF = SoFar, US1 = InUS)
  ), !,
  valvar1(RestInstr, _, PossUS, US1, OutUS, NewSF, OutSF).
valvar1x(get(T, X, _), RestInstr, PossUS, InUS, OutUS, SoFar, OutSF) :-
  (
     notin(X, SoFar) ->
     NewSF = [X|SoFar],
     (T = variable ;  true),
     (is_member(X, PossUS) -> US1 = [X|InUS] ;  US1 = InUS) ;

     (NewSF = SoFar, US1 = InUS)
  ), !,
  valvar1(RestInstr, _, PossUS, US1, OutUS, NewSF, OutSF).
valvar1x(_, RestInstr, PossUS, InUS, OutUS, SoFar, OutSF) :-
  valvar1(RestInstr, _, PossUS, InUS, OutUS, SoFar, OutSF).

disvalvar1((A ;  B), PossUS, InUS, OutUS, SoFar, OutSF) :- !,
  valvar1(A, _, PossUS, InUS, US1, SoFar, Out1),
  disvalvar1(B, PossUS, US1, OutUS, SoFar, Out2), !,
  unionv(Out1, Out2, OutSF).
disvalvar1(B, PossUS, InUS, OutUS, SoFar, OutSF) :-
  valvar1(B, _, PossUS, InUS, OutUS, SoFar, OutSF). % pass 2 

valvar2(V, _, _, SF, SF) :-
  var(V), !.
valvar2([], _, _, SF, SF) :- !.
valvar2([H|T], _, UnSafe, SoFar, OutSF) :-
  valvar2x(H, T, UnSafe, SoFar, OutSF).

valvar2x((A ;  B), RestCode, UnSafe, SoFar, OutSF) :- !,
  valvar2(RestCode, _, UnSafe, SoFar, NewSF), !,
  disvalvar2((A ;  B), UnSafe, NewSF, OutSF).
valvar2x((G - _), RestCode, UnSafe, SoFar, OutSF) :-
  valvar2(RestCode, _, UnSafe, SoFar, NewSF), !,
  valvar2(G, _, UnSafe, NewSF, OutSF).
valvar2x(unify(T, X), RestInstr, UnSafe, SoFar, OutSF) :- !,
  valvar2(RestInstr, _, UnSafe, SoFar, NewSF),
  (
     notin(X, NewSF) ->
     OutSF = [X|NewSF],
     (
        is_member(X, UnSafe) ->
        (T = unsafe_value ;  true) ;

        (T = value ;  true)
     ) ;

     ((T = value ;  true), OutSF = NewSF)
  ), !.
valvar2x(put(T, X, _), RestInstr, UnSafe, SoFar, OutSF) :- !,
  valvar2(RestInstr, _, UnSafe, SoFar, NewSF),
  (
     notin(X, NewSF) ->
     OutSF = [X|NewSF],
     (
        is_member(X, UnSafe) ->
        (T = unsafe_value ;  true) ;

        (T = value ;  true)
     ) ;

     ((T = value ;  true), OutSF = NewSF)
  ), !.
valvar2x(get(T, X, _), RestInstr, UnSafe, SoFar, OutSF) :- !,
  valvar2(RestInstr, _, UnSafe, SoFar, NewSF),
  (
     notin(X, NewSF) ->
     OutSF = [X|NewSF],
     (
        is_member(X, UnSafe) ->
        (T = unsafe_value ;  true) ;

        (T = value ;  true)
     ) ;

     ((T = value ;  true), OutSF = NewSF)
  ), !.
valvar2x(_, RestInstr, UnSafe, SoFar, OutSF) :-
  valvar2(RestInstr, _, UnSafe, SoFar, OutSF).

disvalvar2((A ;  B), UnSafe, SoFar, OutSF) :- !,
  valvar2(A, _, UnSafe, SoFar, Out1),
  disvalvar2(B, UnSafe, SoFar, Out2), !,
  unionv(Out1, Out2, OutSF).
disvalvar2(B, UnSafe, SoFar, OutSF) :-
  valvar2(B, _, UnSafe, SoFar, OutSF).

/*********************************************************************/

/*
         Calculate from unravelled source code the varlist used for calculating
         lifetimes. All goal arguments (variables and atoms) are simply listed.
         For unify goals only the variables are listed.
         Goal arguments are delimited by one or both of arity (Arity) 
         and fence(Name)
         This is determined as follows
     
         1. arity (Arity) allows tempalloc to do more optimal allocation
            It comes before the arguments.
            It is generated for all goals, even built-ins (except unify or
            goals with arity 0, or if all arguments are non-variable).
     
         2. fence(NAme) is used in lifetimes to kill temporaries. 
            It comes after arguments. It is not generated for built-ins 
            or the head of the clause.
*/

varlist([Head|RestCode], [arity(Arity)|Vars]) :-
  functor(Head, _, Arity),
  Head =.. [_|Args],
  linkify(Args, Vars - L), !,
  xvarlist(RestCode, L, []).

xvarlist([X|RestCode], [Dis|Vars], Link) :-
  X = (_ ;  _),
  dislist(X, Dis), !,
  xvarlist(RestCode, Vars, Link).
xvarlist([Goal|RestCode], Vars, Link) :-
  goalsvars(Goal, Vars - L), !,
  xvarlist(RestCode, L, Link).
xvarlist([], Link, Link).

dislist((A ;  B), (AVars ;  BVars)) :-
  xvarlist(A, AVars, []), !,
  dislist(B, BVars).
dislist(B, BVars) :-
  xvarlist(B, BVars, []).

goalsvars(A = S, Vars - Link) :-
  (
     var(S) ->
     SVars = [S] ;

     (list(S) -> SVars = S ;  (functor(S, _, _) -> S =.. [_|SVars]))
  ), !,
  getvars([A|SVars], Vars - Link).
goalsvars(mod$call(Mod, Goal), Vars - Link) :-
  functor(Goal, Name, Arity),
  Goal =.. [Name|Args],
  (Arity = 0 -> Vars = L ;  Vars = [arity(Arity)|V], linkify(Args, V - L)),
  (builtin(Name, Arity) -> L = Link ;  L = [fence(Name)|Link]).
goalsvars(Goal, Vars - Link) :-
  functor(Goal, Name, Arity),
  Goal =.. [Name|Args],
  (Arity = 0 -> Vars = L ;  Vars = [arity(Arity)|V], linkify(Args, V - L)),
  (builtin(Name, Arity) -> L = Link ;  L = [fence(Name)|Link]).

/**********************************************************************/

/*
         Calculate lifetimes of all temp. variables using the varlist
         (Permanents must be allocated beforehand)
         Use fence(_) to forget temporaries.
         Two passes needed - down and back up
*/

lifetime(VarList, LifeList, ForwList, BackList) :-
  ForwList = [[]|_],
  forward(VarList, ForwList, _), !,
  backward(VarList, BackList, []), !,
  map_intersectv(ForwList, BackList, LifeList), !.

                                            % Forward pass: watch data flow !!

forward([X|Rest], [FLeft, FRight|FRest], FLast) :- % In: FLeft, Out: FLast
  var(X), !,
  unionv([X], FLeft, FRight), !,
  forward(Rest, [FRight|FRest], FLast).
forward([fence(_)|Rest], [_, []|FRest], FLast) :- !,
  forward(Rest, [[]|FRest], FLast).
forward([Dis|Rest], [FLeft, FIn, FRight|FRest], FLast) :-
  Dis = (_ ;  _),
  forwdis(Dis, [FLeft, FIn], FRight), !,
  forward(Rest, [FRight|FRest], FLast).
forward([_|Rest], [FLeft, FLeft|FRest], FLast) :-
  forward(Rest, [FLeft|FRest], FLast).
forward([], [FLast], FLast).

forwdis((A ;  B), [FLeft, (AIn ;  BIn)], FRight) :-
  AIn = [FLeft|_],                          % In: FLeft, Out: AIn, BIn, FRight
  forward(A, AIn, ARight),
  forwdis(B, [FLeft, BIn], BRight), !,
  unionv(ARight, BRight, FRight).
forwdis(B, [FLeft, BIn], FRight) :-
  BIn = [FLeft|_],
  forward(B, BIn, FRight).

                       % Backward Pass = watch out for convoluted dataflow !!!

backward([X|Rest], [BLeft, BRight|BRest], BLast) :-
  var(X), !,                                  % In: Blast, Out: BLeft, BRight
  backward(Rest, [BRight|BRest], BLast), !,
  unionv([X], BRight, BLeft).
backward([fence(_)|Rest], [[], L|BRest], BLast) :- !,
  backward(Rest, [L|BRest], BLast).
backward([Dis|Rest], [BLeft, BIn, BRight|BRest], BLast) :-
  Dis = (_ ;  _), !,
  backward(Rest, [BRight|BRest], BLast), !,
  backdis(Dis, [BLeft, BIn, BRight]).
backward([_|Rest], [BLeft, BLeft|BRest], BLast) :-
  backward(Rest, [BLeft|BRest], BLast).
backward([], [BLast], BLast).

backdis((X ;  Y), [BLeft, (XIn ;  YIn), BRight]) :-
  XIn = [XLeft|_],                          % In: BRight, Out: XIn, YIn, BLeft
  backward(X, XIn, BRight),
  backdis(Y, [YLeft, YIn, BRight]), !,
  unionv(XLeft, YLeft, BLeft).
backdis(Y, [BLeft, YIn, BRight]) :-
  YIn = [BLeft|_], !,
  backward(Y, YIn, BRight).

/************************************************************************
**   Temp variable allocation. Uses list created by varlist
**   and lifetime list created by lifetime.
**   Takes the overlap of registers caused by calls into account.
**   The life list does not have to contain any instantiated entries
**
**   Use counter 0 to track register index whose allocate caused
**   a conflict (-1 ==> no conflict)
**
**
**   Use counter 2 to store max index used so far -- note that registers
**   may be used implicitly - in a call where the ith argument is the
**   same as the ith argument of head for example, which argument
**   (register) has not been used before. So we also have to track
**   this.
**
**   We need this number to deal with try or try_else INSIDE a clause
**   e.g. by expanding out ...,(A1 ; A2),... for then the choice point
**   needs to know how many temp variables are in use to stash away.
**   Note that in general, when the try's have been generated to link
**   clauses together (i.e. in compp2.pro) then the number of temps that
**   have to be stashed away is simply the arity of the head of 
**   the clause(s)
************************************************************************/

setNTV(NTV) :-
  cntr_set(2, NTV).

getNTV(NTV) :-
  cntr_get(2, NTV).

setConflictor(I) :-
  cntr_set(0, I).

getConflictor(I) :-
  cntr_get(0, I).

tempalloc([arity(HeadArity)|Vars], [_|Life]) :-
  setConflictor(-1),
  setNTV(HeadArity),                          % use at least this many
  tempa(Vars, Life, 1, HeadArity, [], head), !.

% tempa(VarsToAlloc, Lifes, TryThisTemp, InUseToHere, NotConflict, Where)

tempa(_, [Live|_], N, Max, OK, Place) :-      % fail if there is a conflict 
  (
     Place = body ->
     (N = 1 -> Interval = empty ;  N1 is N - 1, Interval = interval(1, N1)) ;

     (N > Max -> Interval = empty ;  Interval = interval(N, Max))
  ),
  conflict(Live, Interval, I, Severe),
  (notin(I, OK) ;  Severe = not_ok),
  setConflictor(I), !,                        % save offending index and 
  fail.             % backtrack to find alloc which consumed I (in an alloc())
tempa([], _, _, _, _, _) :- !.
tempa([X|Vars], [_, Right|LifeList], N, Max, OK, Place) :-
  var(X),
  is_member(X, Right), !,
  alloc(X, Right, N),                        % try to allocate to an argument 
  (
     N =< Max ->
     N1 is N + 1,
     NewMax = Max,
     (X = x(N) -> NewOK = [N|OK] ;  NewOK = OK) ;

                             % else we are passed the arguments of the arity()

     N1 = 1,
     NewMax = 0,
     NewOK = []
  ),
  tempa(Vars, [Right|LifeList], N1, NewMax, NewOK, Place).

       % on failure of tempa, backtrack to redo the alloc causing the conflict
tempa([X|Vars], [_|LifeList], _, _, _, _) :-
  nonvar(X),
  X = arity(Arity), !,
  update_maxreg(Arity),
  tempa(Vars, LifeList, 1, Arity, [], body).
tempa([X|Vars], [_, In, Right|LifeList], _, _, _, _) :-
  nonvar(X),
  X = (_ ;  _),
  In = (_ ;  _), !,
  distempa(X, In),
  tempa(Vars, [Right|LifeList], 1, 0, [], body).
tempa([X|Vars], [_|LifeList], N, Max, OK, Place) :-
  (
     N =< Max ->
     N1 is N + 1,
     NewMax = Max,
     (nonvar(X), X = x(N) -> NewOK = [N|OK] ;  NewOK = OK) ;

     N1 = 1,
     NewMax = 0,
     NewOK = []
  ),
  tempa(Vars, LifeList, N1, NewMax, NewOK, Place). % Handle disjunctions 

distempa((A ;  B), (ALife ;  BLife)) :- !,    % is this cut OK ???
  tempa(A, ALife, 1, 0, [], body),
  distempa(B, BLife).
distempa(B, BLife) :-
  tempa(B, BLife, 1, 0, [], body).

/*
conflict(Live, interval(L, H), I, ok) :-          
        L =< H, 
        for(I, L, H, 1), 
        is_member(x(I), Live).
*/

             % interval [L, L+1, ..., H] is also considered as live registers.
                   % It is represented as interval(L,H) or as the atom 'empty'

conflict(Live, interval(L, H), I, ok) :-     % succeeds if a register conflict
  L =< H,
  member(x(I), Live),
  I >= L,
  I =< H.
conflict(Live, _, I, not_ok) :-
  conflict(Live, I).

conflict([V|Live], I) :-
  nonvar(V),
  V = x(I),
  is_member(V, Live).
conflict([_|Live], I) :-
  conflict(Live, I).

/*
*   try to allocate variable X to register N -- 
*   Alive contains those registers which are live at the moment 
*   We backtrack into here on an allocation conflict
*/

alloc(X, Alive, N) :-
  notin(x(N), Alive),
  X = x(N).
alloc(X, Alive, N) :-                % alloc fails if cntr 0 not -1 or N - ray
  (getConflictor(-1) ;  getConflictor(N), setConflictor(-1)), !,
  N1 is N + 1,
  update_maxreg(N1),
  alloc(X, Alive, N1).

update_maxreg(I) :-
  getNTV(Max),
  I > Max, !,
  setNTV(I).
update_maxreg(I).

%----------------------------------------------------------------------------%
/**********************************************************************
    **  Peephole optimization: - of several kinds
    **
    **  1    environment size calculations
    **  2    code generation for some built-ins
    **  3    allocate and deallocate instructions
    **  4    last instruction (proceed or execute)
    **  5    customization of instructions
    **  6    filling in #tempvars to be saved by tryor and try_else
    **       (these have been generated by (;) in a clause, and we get
    **        the # of temps by looking at cntr(2))
    **
    ***********************************************************************/

peephole(Code, PCode) :-
  peephole(Code, IntCode, L, 0, _, no_alloc),
  modify_code(IntCode - L, PCode), !.

% peephole( OldCode, NewCode, UnifyCntr, EnvSize, NeedAllocateFlag) 

% remove superfluous gets of voids 
peephole([get(_, A, _)|Code], PCode, L, _, EnvSize, Alloc) :-
  var(A),
  !,
  peephole(Code, PCode, L, 0, EnvSize, Alloc).
  
peephole([put(X,Y,Z),get(_, A, _)|Code], PCode, L, _, EnvSize, Alloc) :-
  var(A),
  !,
  peephole([put(X,Y,Z)|Code], PCode, L, 0, EnvSize, Alloc).

                         % collect unifies of voids and replace by unify_void 
peephole([unify(_, Arg)|Code], PCode, L, N, EnvSize, Alloc) :-
  var(Arg), !,
  N1 is N + 1,
  peephole(Code, PCode, L, N1, EnvSize, Alloc).
peephole(Code, [unify(void, N)|PCode], Link, N, EnvSize, Alloc) :-
  N > 0, !,
  peephole(Code, PCode, Link, 0, EnvSize, Alloc).

              % Insert allocate and deallocate - handle last instruction also 
peephole([call(G, 0)], LastCode, Link, _, 0, Alloc) :- !,
  Tail = [execute(G)|Link],
  (Alloc = yes_alloc -> LastCode = [deallocate|Tail] ;  LastCode = Tail).
peephole([mod_call(M, G, 0)], LastCode, Link, _, 0, Alloc) :- !,
  Tail = [mod_execute(M, G)|Link],
  (Alloc = yes_alloc -> LastCode = [deallocate|Tail] ;  LastCode = Tail).
peephole([], LastCode, Link, _, 0, Alloc) :-
  Tail = [proceed|Link],
  (Alloc = yes_alloc -> LastCode = [deallocate|Tail] ;  LastCode = Tail).
peephole([I|Code], [allocate(EnvSize)|PCode], Link, _, EnvSize, no_alloc) :-
  alloc_needed(I), !,
  peephole([I|Code], PCode, Link, 0, EnvSize, yes_alloc).

                                  % recognize and eliminate superfluous jumps 
peephole([label(Lbl), execute(Lbl)|Code], [execute(Lbl)|PCode], Link, _, 
        EnvSize, Alloc) :- !,
  peephole(Code, PCode, Link, 0, EnvSize, Alloc).
peephole([(fail/0)|Code], [fail|PCode], Link, _, EnvSize, Alloc) :- !,
  peephole(Code, MCode, Link, 0, EnvSize, Alloc),
  f_remove(MCode, PCode).

/*   deal with temp var stashing in choice point -- tmpalloc has set
**   the number of vars to be save in cntr(2) ( = NTV)
*/
peephole([TRY|Code], [TRY|PCode], Link, _, EnvSize, Alloc) :-
  (TRY = try(NTV, Lbl) ;  TRY = tryor(NTV, else, Lbl)),
  (var(NTV) -> getNTV(NTV) ;  true),
  peephole(Code, PCode, Link, 0, EnvSize, Alloc), !.

% optimize unify goals 
peephole([put(variable, R, R), get(A, X, R)|Code], PCode, Link, _, EnvSize, 
        Alloc) :-
  R = x(I),
  (integer(I) ;  I = 0 /*temp*/), !,
  peephole([put(A, X, R)|Code], PCode, Link, 0, M, Alloc),
  (nonvar(X), X = y(N), N > M -> EnvSize = N ;  EnvSize = M).

% second case - both variable are permanent 
peephole([put(A, X, x(0 /*temp*/)), get(B, Y, x(0 /*temp*/))|Code], PCode, 
        Link, _, EnvSize, Alloc) :-
  X = y(N1),
  Y = y(N2), !,
  (
     B = unsafe_value ->
     NewB = value,
     (X == Y -> NewA = unsafe_value ;  NewA = A) ;

     NewA = A,
     NewB = B
  ),
  PCode = [put(NewA, X, x(0 /*temp*/)), get(NewB, Y, x(0 /*temp*/))|MCode],
  peephole(Code, MCode, Link, 0, M, Alloc),
  max_of(N1, M, M2),
  max_of(N2, M2, EnvSize).

% instantiate puts of voids - must come after previous optimizations
peephole([X|Code], PCode, Link, _, EnvSize, Alloc) :-
  X = put(variable, R, R),
  nonvar(R),
  R = x(I), !,
  (I = 0 /*temp*/ -> PCode = MCode ;  PCode = [X|MCode]),
  peephole(Code, MCode, Link, 0, EnvSize, Alloc).

% remove superfluous inits of perm. variables 
peephole([put(value, y(_), x(0 /*temp*/)), I|Code], PCode, Link, _, 
        EnvSize, Alloc) :-
  functor(I, Name, _),
  Name \== get, !,
  peephole([I|Code], PCode, Link, 0, EnvSize, Alloc).

% remove register no-op transfers 
peephole([I|Code], PCode, Link, _, EnvSize, Alloc) :-
  (I = get(variable, R, R) ;  I = put(value, R, R)),
  R = x(_), !,
  peephole(Code, PCode, Link, 0, EnvSize, Alloc).
peephole([get(constant, C, R), put(constant, C, R)|Code], 
        [get(constant, C, R)|PCode], Link, _, EnvSize, Alloc) :- !,
  peephole(Code, PCode, Link, 0, EnvSize, Alloc). % calculate env. sizes 
peephole([I|Code], [I|PCode], Link, _, EnvSize, Alloc) :-
  (I = put(_, R, _) ;  I = get(_, R, _) ;  I = unify(_, R)),
  nonvar(R),
  R = y(N), !,
  peephole(Code, PCode, Link, 0, M, Alloc),
  (N > M -> EnvSize = N ;  EnvSize = M).
peephole([I|Code], [I|PCode], Link, _, EnvSize, Alloc) :-
  (I = call(_, EnvSize) ;  I = mod_call(_, _, EnvSize)),
  peephole(Code, PCode, Link, 0, EnvSize, Alloc).

/* optimize common unify_var x( 0  ) , get_list x( 0  ) sequence -
   replace by one op (used when walking a list)
*/
peephole([unify(variable, x(0 /*temp*/)), 
         get(structure, ('.'/2), x(0 /*temp*/))|Code], 
        [u_var_getlist|PCode], Link, _, EnvSize, Alloc) :-
  peephole(Code, PCode, Link, 0, EnvSize, Alloc). % post transformation -- 
               % makes code for some built-ins in terms of existing functions 
peephole([(Name/Arity)|Code], PCode, Link, _, EnvSize, Alloc) :-
  post_trans(Name, Arity, TCode - Code), !,
  peephole(TCode, PCode, Link, 0, EnvSize, Alloc).

                     % customization of instructions -- no op if not possible 
peephole([I|Code], [CI|PCode], Link, _, EnvSize, Alloc) :- !,
  customize(I, CI), !,
  peephole(Code, PCode, Link, 0, EnvSize, Alloc).

                     % remove code until encountering a label, retry or trust 

f_remove(V, V) :-
  var(V).
f_remove([Instr|Code], [Instr|Code]) :-
  functor(Instr, N, _),
  (N = label ;  N = retry ;  N = trust), !.
f_remove([_|Code], RCode) :-
  f_remove(Code, RCode).                      
  
% table of built-ins with code, NOTE! they must also be
% listed in builtins.pro.

post_trans(var, 1, [switch_on_term(fail, fail, fail)|L] - L).
post_trans(nonvar, 1, 
          [switch_on_term(Lbl, Lbl, Lbl), fail, label(Lbl)|L] - L).
post_trans(atomic, 1, 
          [switch_on_term(Lbl, fail, fail), fail, label(Lbl)|L] - L).
post_trans(nonatomic, 1, 
          [switch_on_term(fail, Lbl, Lbl), label(Lbl)|L] - L).
post_trans(list, 1, 
          [switch_on_term(fail, Lbl, fail), fail, label(Lbl)|L] - L).
post_trans(nonlist, 1, [switch_on_term(Lbl, fail, Lbl), label(Lbl)|L] - L).
post_trans(structure, 1, 
          [switch_on_term(fail, fail, Lbl), fail, label(Lbl)|L] - L).
%post_trans(composite, 1, 
%          [switch_on_term(fail, Lbl, Lbl), fail, label(Lbl)|L] - L).
post_trans(compound, 1, 
          [switch_on_term(fail, Lbl, Lbl), fail, label(Lbl)|L] - L).
%post_trans(simple, 1, [switch_on_term(Lbl, fail, fail), label(Lbl)|L] - L).
post_trans(repeat, 0, [try(NTV, Lbl), label(Lbl)|L] - L) :-
  getNTV(NTV).                                % customize one instruction 

customize(get(structure, ('.'/2), B), get_list(B)).
customize(put(structure, ('.'/2), B), put_list(B)).
% a bug here, used to have [] in head of clause, but that
% unifies with both '[]' and `[]`, creating strange bugs
% for users of `[]` in their code.
customize(put(constant, NIL, A), put_nil(A)) :- NIL = [], atom(NIL).
customize(get(constant, NIL, A), get_nil(A)) :- NIL = [], atom(NIL).
customize(unify(constant, NIL), unify_nil) :- NIL = [], atom(NIL).
customize(Instr, Instr).

% succeeds if an allocate is needed before instruction I 

alloc_needed(I) :-
  functor(I, Name, _),
  (
     Name = call ;
     Name = mod_call ;
     Name = try ;
     Name = tryor ;
     Name = cut ;
     Name = cut64
  ).
alloc_needed(I) :-
  (I = get(_, V, _) ;  I = put(_, V, _) ;  I = unify(_, V)),
  nonvar(V),
  V = y(_).

% modify_code(InCode, OutCode) .. used to fix up so inefficiences
% due to the code allocation scheme - in particular this juices
% up append/3 and its near siblings (e.g. delete/3 in zebra)

modify_code([get_list(x(I)), unify(variable, x(I)), unify(variable, x(4)), 
            get_list(x(3)), unify(value, x(I)), unify(variable, x(3)), 
            put(value, x(4), x(I)), execute(N/3)|L] - L, 
           [get_list(x(I)), unify(variable, x(4)), unify(variable, x(I)), 
           get_list(x(3)), unify(value, x(4)), unify(variable, x(3)), 
           execute(N/3)|L] - L) :-
  (I = 1 ;  I = 2), !.
modify_code(X, X).

%----------------------------------------------------------------------------%
% Local utils
% utils.pro -- various utility routines
                                   % Set utilities - a 'v means no unification

notin(X, L) :-
  is_member(X, L), !,
  fail.
notin(_, _).

unionv(S1, S2, S1) :-
  S1 == S2, !.
unionv([X|S1], S2, Res) :-
  is_member(X, S2), !,
  unionv(S1, S2, Res).
unionv([X|S1], S2, [X|Res]) :-
  unionv(S1, S2, Res).
unionv([], S, S).

diffv([X|S1], S2, Res) :-
  is_member(X, S2), !,
  diffv(S1, S2, Res).
diffv([X|S1], S2, [X|Res]) :-
  diffv(S1, S2, Res).
diffv([], _, []).

intersectv([X|Set1], Set2, Res) :-
  is_member(X, Set1), !,
  intersectv(Set1, Set2, Res).
intersectv([X|Set1], Set2, Res) :-
  not(is_member(X, Set2)), !,
  intersectv(Set1, Set2, Res).
intersectv([X|Set1], Set2, [X|Res]) :-
  intersectv(Set1, Set2, Res).
intersectv([], _, []).                        % List processing goodies

nonlist(Term) :-
  list(Term), !,
  fail.
nonlist(_).

append([], L, L).
append([H|L1], L2, [H|Res]) :-
  append(L1, L2, Res).

member(X, [X|_]).
member(X, [_|L]) :-
  member(X, L).

reverse(A, Z) :-
  reverse(A, [], Z).

reverse([], Z, Z).
reverse([A|X], SoFar, Z) :-
  reverse(X, [A|SoFar], Z).

last([Last], Last) :- !.
last([_|List], Last) :-
  last(List, Last).

map_unionv([A|L1], [B|L2], [C|L3]) :-
  unionv(A, B, C), !,
  map_unionv(L1, L2, L3).
map_unionv([], [], []).           % linkify converts list into difference list

linkify([], Link - Link).
linkify([A|List], [A|DiffList] - Link) :-
  linkify(List, DiffList - Link).

% extract all variable terms from input list and put them in difference list

getvars(V, Rest - Link) :-
  (var(V), Rest = [V|Link] ;  nonlist(V), Rest = Link), !.
getvars([V|List], [V|Vars] - Link) :-
  var(V), !,
  getvars(List, Vars - Link).
getvars([X|List], Vars - Link) :-
  nonvar(X), !,
  getvars(List, Vars - Link).
getvars([], Link - Link).

map_intersectv([X|XRest], [Y|YRest], [Z|ZRest]) :-
  X = (_ ;  _), !,
  mapdis(X, Y, Z), !,
  map_intersectv(XRest, YRest, ZRest).
map_intersectv([X|XRest], [Y|YRest], [Z|ZRest]) :-
  intersectv(X, Y, Z),
  map_intersectv(XRest, YRest, ZRest).
map_intersectv([], [], []).

mapdis((X ;  XRest), (Y ;  YRest), (Z ;  ZRest)) :-
  map_intersectv(X, Y, Z), !,
  mapdis(XRest, YRest, ZRest).
mapdis(X, Y, Z) :-
  map_intersectv(X, Y, Z).

max_of(N, M, N) :-
  N > M, !.
max_of(N, M, M).

ctr_add(I, T) :-
  cntr_get(I, T1),
  T2 is T1 + T,
  cntr_set(I, T2).

/*
time_it(X) :-
%        timer(T1),
   (call(X) ;  true), !,
%        timer(T2),
   T2 is 0,
   T1 is 0,
   ctr_add(9, T2 - T1).
*/

:- end_body(amzi_compiler).

