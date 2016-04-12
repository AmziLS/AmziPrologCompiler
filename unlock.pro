:- import(date_time).
:- import(amzi_register).

main :-
   now_locked,
   greetings,
   get_info(UserName, OrgName, SerialNum, UnlockCode),
   check_release(SerialNum),
   write(`unlocking...`), nl,
   amzi_directory(Adir),
   stringlist_concat([Adir, `abin/`, `unlock.xpl`], UNLOCK),
   once unlock(UNLOCK, UserName, OrgName, SerialNum, UnlockCode),
   stringlist_concat([Adir, `abin/`, `alis.xpl`], ALIS),
   once unlock(ALIS, UserName, OrgName, SerialNum, UnlockCode),
   stringlist_concat([Adir, `abin/`, `acmp.xpl`], ACMP),
   once unlock(ACMP, UserName, OrgName, SerialNum, UnlockCode),
   stringlist_concat([Adir, `abin/`, `aidl.xpl`], AIDL),
   % AIDL only on Windows, so no error if it doesn't work
   ( catch(unlock(AIDL, UserName, OrgName, SerialNum, UnlockCode), _, true) ; true ),
   write(`License upgrade successful!`), nl, !.
main :-
   unlock_err(Err),
   nl,
   write(Err),
   nl.

ide_unlock(UserName, OrgName, SerialNum, UnlockCode, `ok`) :-
   now_locked,
   check_release(SerialNum),
   amzi_directory(Adir),
   stringlist_concat([Adir, `abin/`, `unlock.xpl`], UNLOCK),
   once unlock(UNLOCK, UserName, OrgName, SerialNum, UnlockCode),
   stringlist_concat([Adir, `abin/`, `alis.xpl`], ALIS),
   once unlock(ALIS, UserName, OrgName, SerialNum, UnlockCode),
   stringlist_concat([Adir, `abin/`, `acmp.xpl`], ACMP),
   once unlock(ACMP, UserName, OrgName, SerialNum, UnlockCode),
   stringlist_concat([Adir, `abin/`, `aidl.xpl`], AIDL),
   once unlock(AIDL, UserName, OrgName, SerialNum, UnlockCode).
ide_unlock(_, _, _, _, Err) :-
   unlock_err(Err).

now_locked :-
   op$en(l),
   !.
now_locked :-
   assert(unlock_err(`Product is already licensed.`)),
   !, fail.

unl$defined :-
  defined(amzi_register:unl$sta/1).

op$en(Status) :-
  unl$defined, !,
  unl$sta(Status).
op$en(i).

who$ami(User, Org, Product, Platform, Version) :-
  unl$defined, !,
  unl$gri(User, Org, _, _, _),
  unl$gid(Product, Platform, Version, _, _).
who$ami(`System`, `Amzi! inc.`, `APX`, ``, ``).

who$ami(User, Org, Product, Platform, Version, SerialNo) :-
  unl$defined, !,
  unl$gri(User, Org, _, _, _),
  unl$gid(Product, Platform, Version, _, SerialNo).
who$ami(`System`, `Amzi! inc.`, `APX`, ``, ``, ``).

greetings :-
   version(Ver),
   write(`\nAmzi! Prolog + Logic Server `), write(Ver), 
   write(`\nCopyright (c)1992-2002 Amzi! inc. All Rights Reserved.\n`), nl,
   op$en(X),
   greetings(X).

greetings(i) :-
   !,
   write(`\n  ****************************************************\n`), 
   write(`    Internal development version, Amzi! use only.\n`), 
   write(`****************************************************\n`).
greetings(l) :-
   !,
   write(`\n  ****************************************************\n`), 
   write(`  This limited free product is licensed for academic,\n`),
   write(`  personal or 90-day evaluation use (see license\n`),
   write(`  agreement in docs for details).  To purchase a\n`),
   write(`  Standard or Professional License, or obtain a\n`),
   write(`  registered Academic License, e-mail sales@amzi.com.\n`),
   write(`  ****************************************************\n`).
greetings(r) :-
   !,
   who$ami(User, Org, Prod, Plat, Ver),
   product_name(Prod, ProdName),
   write(`\n  ****************************************************\n`),
   tab(2), write(ProdName), tab(1),
   write(`licensed version for:\n`),
   tab(2), write(User), write(`, at `), write(Org), nl,
   write(`  ****************************************************\n`).
greetings(t) :-
   !,
   who$ami(User, Org, Prod, Plat, Ver),
   product_name(Prod, ProdName),
   unl$ctm(XMonth, XMonthName, XDay, XYear),
   write(`\n  ****************************************************\n`),
   tab(2), write(ProdName), tab(1),
   write(`licensed version for:\n`),
   tab(2), write(User), write(`, at `), write(Org), nl,
   write(`  Maintenance and support good until: `),
   write(XYear), tab(1), write(XMonthName), tab(1), write(XDay), nl,
   write(`  ****************************************************\n`).
greetings(x) :-
   !,
   who$ami(User, Org, Prod, Plat, Ver),
   product_name(Prod, ProdName),
   unl$ctm(XMonth, XMonthName, XDay, XYear),
   write(`\n  ****************************************************\n`), 
   tab(2), write(ProdName), tab(1),
   write(`licensed version for:\n`),
   tab(2), write(User), write(`, at `), write(Org), nl,
   write(`  Maintenance has expired!  Contact Amzi! to renew.`),
   write(`  ****************************************************\n`).

product_name('APS', `Standard`).
product_name('APX', `Professional`).
product_name('AP1', `Academic/Personal`).


get_info(UserName, OrgName, SerialNum, UnlockCode) :-
   write(`Answer the prompts with the information from your registration form.\n`),
   write(`Include the dashes as given in the serial number and unlock code.\n`),
   write(`User Name:     `), 
   read_string(UserName),
   write(`Organization:  `), 
   read_string(OrgName),
   write(`Serial Number: `), 
   read_string(SerialNum),
   write(`Unlock Code:   `), 
   read_string(UnlockCode).
   
check_release(SerialNum) :-
   unl$pid(SerialNum, _, _, Ver, _, _, XMon, _, XDay, XYear),
   !,
   check_version(Ver),
   check_date(XYear, XMon, XDay).

check_version(Ver) :-
   sub_string(Ver, 1, 1, UnlMajor),
   sub_string(Ver, 3, 1, UnlMinor),
   version_build(MM, _, _),  % returns version, build, e.g. 61, 80
   string_term(MMStr, MM),
   sub_string(MMStr, 1, 1, Major),
   sub_string(MMStr, 2, 1, Minor),
   !,
   (UnlMajor @>= Major ->
      true
      ;
      stringlist_concat([`Unlock code valid for version: `, UnlMajor, `\n`,
            `Can't unlock this version with it: `, Major], Err),
      assert(unlock_err(Err)),
      fail
   ).

check_date(0,0,0) :-
   !.
check_date(XYear, XMon, XDay) :-
   version_build(_, _, VDATESTR),
   datetime_string(DT, _, VDATESTR),
   datetime_date_time(DT, VDate, _),
   date_create(XYear, XMon, XDay, XDate),
   !,
   (date_compare(VDate, =<, XDate) ->
      true
      ;
      date_string(VDate, 'd mon y', VDStr),
      date_string(XDate, 'd mon y', XDStr),
      stringlist_concat([`Version date past unlock expiration date\n`,
         `Version date: `, VDStr, `\n`,
         `Expiration date: `, XDStr], Err),
      assert(unlock_err(Err)),
      fail
   ).

unlock(FileS, UserS, OrgS, ProdS, KeyS) :-
   unl$rid(FileS, UserS, OrgS, $$, ProdS, KeyS).
unlock(_,_,_,_,_) :-
   assert(unlock_err(
     `Bad unlock code, make sure each item\nis entered exactly as shown on registration letter.`)),
   fail.