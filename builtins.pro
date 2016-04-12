%-*-Prolog-*-  
% builtins indented on 12/5/1999 by 'JOLI' 1.0.


%----------------------------------------------------------------------
% builtins.pro
%
% Copyright (c) 1992-2002 Amzi! inc.  All Rights Reserved.
%
% $Log: builtins.pro,v $
% Revision 1.1.1.1  2003/09/11 02:15:10  dennis
% Starting release 7.0
%
% Revision 1.9  2002/05/15 16:59:06  dennis
% Final fixes for last 6.1 build, 80
%
% Revision 1.8  2002/04/25 03:42:22  dennis
% more documentation, logicbase.htm, and some fiddling with sources
%
% Revision 1.7  2002/02/21 21:08:31  dennis
% changed to floats/single for number defaults
%
% Revision 1.6  2001/02/08 22:56:45  dennis
% string bug fixes, modularized compiler and listener
%
% Revision 1.5  2001/01/30 17:22:04  dennis
% made list.pro a module, used by compiler
%
% Revision 1.4  2001/01/30 16:47:28  dennis
% Made, after many trials, alib into amzi_system module.
%
% Revision 1.3  2001/01/11 01:49:22  dennis
% Implemented rest of import/export and metapredicates, working
% for test cases.
%
% Revision 1.2  2001/01/05 06:05:54  dennis
% fixed minor discrepancies
%
% Revision 1.1.1.1  2000/12/29 02:18:05  dennis
% moved to a6
%
% Revision 1.14  2000/11/30 15:47:27  ray
% made arg, nth and prime bilateral.
% made real length and exponent 12 bits for maximum range
%
% Revision 1.13  2000/10/21 03:02:42  dennis
% temp fix of .pro.pro problem in compiler
%
% Revision 1.12  2000/10/01 16:20:03  dennis
% cleaned up modules, ddb, got defined, abolish and friends working
%
% Revision 1.11  2000/09/27 01:42:02  dennis
% implemented listing, needed current_predicate, predicate_property,
% and current_module
%
% Revision 1.10  2000/09/25 02:11:18  dennis
% first version of modules working, runs the modular version of
% duck world.  still needs import and export.  release 6.1.1
%
% Revision 1.9  2000/09/15 21:42:24  dennis
% 12->13
%
% Revision 1.8  2000/09/02 02:10:19  dennis
% new version of get$db replacing dbrefgen for getting clauses
% from dynamic database
%
% Revision 1.7  2000/08/26 00:32:04  dennis
% Merged with ray's changes for new numbers and ISO standard features.
% Added new AtomTable, Dynamic Database and operators based on STL
% maps as basis for ISO modules.
%
% Revision 1.6  2000/05/14 03:52:31  dennis
% a5-1-8 replaced ddbatom with atomdb and one 'user' module
%
% Revision 1.4  2000/03/28 01:05:14  dennis
% merged Ray's changes with bigdig.  bigdig is at point where
% new Cell class is used, but there are no modules in the system.
%
% Revision 1.3.2.2  2000/03/08 04:11:59  dennis
% builtin.cpp compiles
%
% Revision 1.3.2.1  2000/02/26 20:56:12  dennis
% Removed local atoms from compiler, and old module support, so
% compiler and listener are all global for now.  Also made member/2
% and friends built-ins as well as the bug predicates.
%
%
%----------------------------------------------------------------------

:- body(amzi_compiler).

% built-ins which do not destroy any arg registers
% built-ins are C - code predicates

builtin(G) :-
   functor(G, F, A),
   builtin(F, A).

% The following are converted to in-line P-Codes by the optimizer
% which needs to think they are builtins

builtin(!, 0).
builtin(true, 0).
builtin(fail, 0).
builtin(repeat, 0).
builtin(var, 1).
builtin(list, 1).
builtin(nonvar, 1).
builtin(atomic, 1).
builtin(structure, 1).
builtin(compound, 1).

:- end_body(amzi_compiler).

