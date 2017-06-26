%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: nf_closure.pl,v 1.2 1995/04/10 19:24:50 gerd Exp $
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

:- module_interface(nf_closure). /*%-------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.2 $}

\PL*/
:- export closure/2,
	  free_variables/2.
:- begin_module(nf_closure).

:-	lib(eq_member),
	lib(nf_intern).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate closure/2(+Formula, ?New).

Built the existential closure of the formula |Formula| and unify the result
with |New|.

\PL*/
closure(Formula,New) :-
	free_variables(Formula,Vars),
	( Vars == [] ->
	    New = Formula
	;   New = exists(Vars,Formula)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate free_variables/2(+Term, ?VarList).

This predicate takes a formula |Term| in internal representation and extracts
the list |VarList| of the free variables in it.

\PL*/
free_variables(predicate(L),V) :-
	term_variables(L,V).
free_variables(true,[]).
free_variables(false,[]).
free_variables(not(F),V) :-
	free_variables(F,V).
free_variables(exists(QV,F),V) :-
	free_variables(F,V1),
	eq_subtract(V1,QV,V).
free_variables(forall(QV,F),V) :-
	free_variables(F,V1),
	eq_subtract(V1,QV,V).
free_variables((A and B),V) :-
	free_variables(A,VA),
	free_variables(B,VB),
	eq_union(VA,VB,V).
free_variables((A or B),V) :-
	free_variables(A,VA),
	free_variables(B,VB),
	eq_union(VA,VB,V).
free_variables((A implies B),V) :-
	free_variables(A,VA),
	free_variables(B,VB),
	eq_union(VA,VB,V).
free_variables((A iff B),V) :-
	free_variables(A,VA),
	free_variables(B,VB),
	eq_union(VA,VB,V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
