%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: equality_axioms.pl,v 1.5 1995/03/06 23:04:17 gerd Exp $
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

:- module_interface(equality_axioms). /*%--------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.5 $}

\PL*/
:- export equality_axioms/2.

:- begin_module(equality_axioms).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
info(filter,
	"$Revision: 1.5 $",
	"Filter to add the equality axioms to a matrix.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(op_def),
	lib(options),
	lib(literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic function_symbol/2,
	   predicate_symbol/2.
:- define_option 'equality_axioms:equality_substitutivity' = off.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate equality_axioms/2(+Stream, +OutStream).


\PL*/
equality_axioms(Stream,OutStream) :-
	retract_all(function_symbol(_,_)),
	retract_all(predicate_symbol(_,_)),
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	;   Term = (# _) ->
	    writeclause(OutStream,Term),
	    fail
	;   analyze_clause(Term),
	    writeclause(OutStream,Term),
	    fail
	),
	generate_axioms(OutStream),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate analyze_clause/1(+Clause).

This predicate analyzes the given clause and stores the pdicate and function
symbols in the Prolog data base. For this purpose the dynamic predicates
|predicate_symbol/2| and |function_symbol/2| are used. The predicate acts
according to the structural definition  of the \ProTop{} syntax.

\PL*/
analyze_clause((Head:-Tail)) :-
	!,
	analyze_clause(Head),
	analyze_clause(Tail). 
analyze_clause((:-Tail)) :-
	!,
	analyze_clause(Tail).
analyze_clause((?-Tail)) :-
	!,
	analyze_clause(Tail).
analyze_clause((Head<-Tail)) :-
	!,
	analyze_clause(Head),
	analyze_clause(Tail).
analyze_clause(([])) :-
	!.
analyze_clause([Head|Tail]) :-
	!,
	analyze_clause(Head),
	analyze_clause(Tail).
analyze_clause((Head,Tail)) :-
	!,
	analyze_clause(Head),
	analyze_clause(Tail).
analyze_clause((Head;Tail)) :-
	!,
	analyze_clause(Head),
	analyze_clause(Tail).
analyze_clause((_::Tail)) :-
	!,
	analyze_clause(Tail).
analyze_clause(--Literal) :- 
	!,
	store_literal(Literal).
analyze_clause(-Literal) :-
	!,
	store_literal(Literal).
analyze_clause(++Literal) :-
	!,
	store_literal(Literal).
analyze_clause(Literal) :-
	store_literal(Literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate store_literal/1(+Literal).

\PL*/
store_literal(Literal) :-
	functor(Literal,F,A), 
	(predicate_symbol(F,A) -> true; assert(predicate_symbol(F,A))),
	Literal =.. [_|Args],
	analyze_term_list(Args).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate analyze_term_list/1(+List).

\PL*/
analyze_term_list([]).
analyze_term_list([Head|Tail]) :- 
	( var(Head) ->
	    true
	;
	    functor(Head,F,A),
	    (function_symbol(F,A) -> true;  assert(function_symbol(F,A))),
	    Head =.. [_|Args],
	    analyze_term_list(Args)
	),
	analyze_term_list(Tail).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate generate_axioms/1(+Stream).

This predicate uses the facts for the predicates |predicate_symbol/2| and
|function_symbol/2| to generate the instanciated axiom schemata for
equality. Those axioms are written to the output stream |Stream| as clauses.


\PL*/
generate_axioms(OutStream):- 
	writeclause(OutStream,(X=X)),
	writeclause(OutStream,(X=Z :- X=Y, Y=Z)),
	writeclause(OutStream,(X=Y :- Y=X)),
	(   predicate_symbol(F,A),
	    A=\=0,
	    F/A \= (=)/2,
	    functor(Term1,F,A),
	    functor(Term2,F,A),
	    make_substitutivity(Term1,Term2,A,Goals),
	    writeclause(OutStream,(Term1:-Term2,Goals)),
	    fail
	;
	    is_option('equality_axioms:equality_substitutivity'),
	    writeclause(OutStream,((X=Y):-X=X1,Y=Y1,X1=Y1)),
	    fail
	;
	    function_symbol(F,A),
	    A=\=0,
	    functor(Term1,F,A),
	    functor(Term2,F,A),
	    make_substitutivity(Term1,Term2,A,Goals),
	    writeclause(OutStream,(Term1=Term2:-Goals)),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_substitutivity/4(+Term1, +Term2, +Pos, ?Code).

\PL*/
make_substitutivity(_,_,0,true):-
	!.
make_substitutivity(Term1,Term2,1,(X=Y)):-
	!,
	arg(1,Term1,X),
	arg(1,Term2,Y).
make_substitutivity(Term1,Term2,Pos,(X=Y,Goals)):-
	arg(Pos,Term1,X),
	arg(Pos,Term2,Y),
	Pos1 is Pos-1,
	make_substitutivity(Term1,Term2,Pos1,Goals).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
