%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: nf_def_pg.pl,v 1.1 1995/04/24 21:29:11 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1995 Gerd Neugebauer
%%% 
%%% Net: gerd@imn.th-leipzig.de
%%% 
%%%****************************************************************************

:- module_interface(nf_def_pg). /*%--------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.1 $}

\PL*/
:- export def_pg/2.
:- begin_module(nf_def_pg).

:-	lib(nf_intern),
	lib(nf_closure).

:-	make_local_array(counter),
	setval(counter,1).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
def_pg(Formula,NF) :-
	pg_plus(Formula,LF,Fplus),
	NF = (LF and Fplus).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
new_name(Prefix,Name) :-
	getval(counter,Counter),
	incval(counter),
	concat_atom(["@_",Prefix,"_",Counter],Name).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
pg_plus(true,true,true).
pg_plus(false,false,true).
pg_plus(predicate(A),predicate(A),true).
pg_plus(not(A),LA,Aplus) :-
	pg_minus(A,LA,Aplus).
pg_plus((A and B),L_AandB,New) :-
	new_name(and,Name),
	pg_plus(A,LA,Aplus),
	pg_plus(B,LB,Bplus),
	free_variables((LA and LB),Vars),
	L =.. [Name|Vars],
	L_AandB = predicate(L),
	conjunction(Aplus, Bplus, Ap_and_Bp ),
	conjunction((L_AandB implies ( LA and LB )),Ap_and_Bp,New).
pg_plus((A or B),L_AorB,New) :-
	new_name(or,Name),
	pg_plus(A,LA,Aplus),
	pg_plus(B,LB,Bplus),
	free_variables((LA and LB),Vars),
	L =.. [Name|Vars],
	L_AorB = predicate(L),
	conjunction(Aplus, Bplus,Ap_and_Bp ),
	conjunction((L_AorB implies ( LA or LB )),Ap_and_Bp,New).
pg_plus((A implies B),L_AimpliesB,New) :-
	new_name(implies,Name),
	pg_minus(A,LA,Aminus),
	pg_plus(B,LB,Bplus),
	free_variables((LA and LB),Vars),
	L =.. [Name|Vars],
	L_AimpliesB = predicate(L),
	conjunction(Aminus, Bplus,Ap_and_Bp ),
	conjunction((L_AimpliesB implies ( LA implies LB )),Ap_and_Bp,New).

pg_plus(exists(X,A),L_existsA,New) :-
	new_name(exists,Name),
	pg_plus(A,LA,Aplus),
	free_variables(exists(X,LA),Vars),
	L =.. [Name|Vars],
	L_existsA = predicate(L),
	conjunction(Aplus,(L_existsA implies exists(X,LA)),New).
pg_plus(forall(X,A),L_forallA,New) :-
	new_name(forall,Name),
	pg_plus(A,LA,Aplus),
	free_variables(forall(X,LA),Vars),
	L =.. [Name|Vars],
	L_forallA = predicate(L),
	conjunction(Aplus,(L_forallA implies forall(X,LA)),New).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
pg_minus(true,true,true).
pg_minus(false,false,true).
pg_minus(predicate(A),predicate(A),true).
pg_minus(not(A),LA,Aminus) :-
	pg_plus(A,LA,Aminus).
pg_minus((A and B),L_AandB,New) :-
	new_name(and,Name),
	pg_minus(A,LA,Aminus),
	pg_minus(B,LB,Bminus),
	free_variables((LA and LB),Vars),
	L =.. [Name|Vars],
	L_AandB = predicate(L),
	conjunction(Aminus, Bminus, Ap_and_Bp ),
	conjunction((L_AandB implies ( LA and LB )),Ap_and_Bp,New).
pg_minus((A or B),L_AorB,New) :-
	new_name(or,Name),
	pg_minus(A,LA,Aminus),
	pg_minus(B,LB,Bminus),
	free_variables((LA and LB),Vars),
	L =.. [Name|Vars],
	L_AorB = predicate(L),
	conjunction(Aminus, Bminus,Ap_and_Bp ),
	conjunction((L_AorB implies ( LA or LB )),Ap_and_Bp,New).
pg_minus((A implies B),L_AimpliesB,New) :-
	new_name(implies,Name),
	pg_plus(A,LA,Aplus),
	pg_minus(B,LB,Bminus),
	free_variables((LA and LB),Vars),
	L =.. [Name|Vars],
	L_AimpliesB = predicate(L),
	conjunction(Aplus, Bminus,Ap_and_Bp ),
	conjunction((L_AimpliesB implies ( LA implies LB )),Ap_and_Bp,New).

pg_minus(exists(X,A),L_existsA,New) :-
	new_name(exists,Name),
	pg_minus(A,LA,Aminus),
	free_variables(exists(X,LA),Vars),
	L =.. [Name|Vars],
	L_existsA = predicate(L),
	conjunction(Aminus,(L_existsA implies exists(X,LA)),New).
pg_minus(forall(X,A),L_forallA,New) :-
	new_name(forall,Name),
	pg_minus(A,LA,Aminus),
	free_variables(forall(X,LA),Vars),
	L =.. [Name|Vars],
	L_forallA = predicate(L),
	conjunction(Aminus,(L_forallA implies forall(X,LA)),New).








self_test(File) :-
	open(File,read,Stream),
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	;   writeln(Term),
	    def_pg(Term,NF),
	    printf("------\n%w\n%w\n",[Term,NF]),
	    fail
	),
	close(Stream).



/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
