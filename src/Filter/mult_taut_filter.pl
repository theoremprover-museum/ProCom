%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: mult_taut_filter.pl,v 1.3 1995/04/24 21:29:11 gerd Exp $
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

:- module_interface(mult_taut_filter). /*%-------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}

This filter performs some trivial reductions on the clause level. Thus
literals or clauses can be omitted. It is not necessary to built the
connection graph for those literals and clauses.

The following reductions are performed:

\begin{description}
\item[mult] Multiple literals in one clause can be reduced to a single
  occurrence of the multiple literals. The comparison of the literals have to
  be really identical, {\em not}\/ unifyable.
\item[taut] A clause containing a literal and its negation can not contribute
  to a minimal proof. Thus it can be deleted. The set of solutions is not
  affected by this reduction.
\end{description}

\PL*/
:- export mult_taut_filter/2.

:- begin_module(mult_taut_filter).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Our identification as a filter for \ProTop.

\PL*/
info(filter,"$Revision: 1.3 $","Apply trivial reductions.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We need some modules from the system.

\PL*/
:-	lib(op_def),
	lib(eq_member),
	lib(literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate mult_taut_filter/2(+Stream, +OutStream).

Terms are transfered from the input stream |Stream| to the output stream
|OutStream| until the end of file is found.

\PL*/
mult_taut_filter(Stream,OutStream) :-
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	;   process_clause(Term,OutTerm),
	    writeclause(OutStream,OutTerm),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate mult_taut/2(Literals, Reduced).

\PL*/
mult_taut([],[]).
mult_taut([H|T],[H1|T2]) :-
	mult_taut(T,T1),
	merge_literal_signs(-H,NegH),
	\+ eq_member(NegH,T1),
	merge_literal_signs(H,H1),
	eq_subtract(T1,[H1],T2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate process_clause/2(Literals, Normalized).

\PL*/
process_clause((#A),(#A)) :-
	!.
process_clause([],[]) :-
	!.
process_clause([H|T],Result) :-
	!,
	mult_taut([H|T],Result).
process_clause((H:-T),Result) :-
	!,
	disjunction_2_list(H,L,L2),
	conjunction_2_list(T,[],L),
	process_clause(L2,Result).
process_clause((H<-T),Result) :-
	!,
	disjunction_2_list(H,L,L2),
	conjunction_2_list(T,[],L),
	process_clause(L2,Result).
process_clause((L::C),(L::C2)) :-
	!,
	process_clause(C,C2).
process_clause((:- A),Result) :-
	!,
	conjunction_2_list(A,[],L),
	process_clause(L,Result).
process_clause((<- A),Result) :-
	!,
	conjunction_2_list(A,[],L),
	process_clause(L,Result).
process_clause((?- []),(?-[])) :-
	!.
process_clause((?- A),(?-Result)) :-
	functor(A,'.',2),
	!,
	process_clause(A,Result).
process_clause((?- A),(?-Result)) :-
	!,
	conjunction_2_list(A,[],L),
	process_clause(L,Result).
process_clause(A,Result) :-
	!,
	disjunction_2_list(A,[],L),
	process_clause(L,Result).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate disjunction_2_list/3().

\PL*/
disjunction_2_list((A;B),L,DL) :-
	!,
	disjunction_2_list(A,LB,DL),
	disjunction_2_list(B,L,LB).
disjunction_2_list((A,B),L,DL) :-
	!,
	disjunction_2_list(A,LB,DL),
	disjunction_2_list(B,L,LB).
disjunction_2_list(A,L,[A2|L]) :-
	merge_literal_signs(A,A2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate conjunction_2_list/3().

\PL*/
conjunction_2_list((A,B),L,DL) :-
	!,
	conjunction_2_list(A,LB,DL),
	conjunction_2_list(B,L,LB).
conjunction_2_list(A,L,[A2|L]) :-
	merge_literal_signs(-A,A2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
