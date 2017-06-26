%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: path-regular.pl,v 1.3 1995/01/11 09:15:45 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1994 Gerd Neugebauer
%%% 
%%% Net: gerd@imn.th-leipzig.de
%%% 
%%%****************************************************************************
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}

This library provides path handling routines. It uses the date structure of a
pair {\em Pos-Neg}\/ to store the positive and negative literals respectivly.
In addition the coroutining facility of \eclipse{} is used to (partially)
implement regularity constraints

\Predicate empty_path/1(?EmptyPath).

This predicate unifies its argument with the empty path.

\PL*/
:- expand_predicate(empty_path/1).
empty_path([]-[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_on_path/3(Literal,Path,NewPath).

\PL*/
:- expand_predicate(put_on_path/3).
put_on_path(Literal,P-N,Path) :-
	functor(Literal,F,_),
	( name(F,[126|_]) ->
	    Path = P-[Literal|N]
	;   Path = [Literal|P]-N
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_on_path/2(Literal,Path).

\PL*/
:- expand_predicate(is_on_path/2).
is_on_path(Literal,P-N) :-
	functor(Literal,F,_),
	( name(F,[126|_]) ->
	    sound_member(Literal,N)
	;   sound_member(Literal,P)
	).
:- require_predicate(sound_member/2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_identical_on_path/2(Literal,Path).

See if an identical pair of predicates is in the path.	This is done utilizing
the delayed clauses of \eclipse{} Prolog.  If the Literal and an element of
the path are unifyable, i.e. if the may be instanciated to the same term, then
a delayed goal is generated.

The |\+| is used to avoid bindings when trying the unification.

\PL*/
:- expand_predicate(is_identical_on_path/2).
is_identical_on_path(Literal,P-N) :-
	functor(Literal,F,_),
	( name(F,[126|_]) ->
	    \+ add_regularity_constraints(N,Literal)
	;   \+ add_regularity_constraints(P,Literal)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_regularity_constraints/2(List,Literal).

\PL*/
%:- set_flag(coroutine,on).
add_regularity_constraints([],Literal).
add_regularity_constraints([Head|Tail],Literal) :-
	( Head == Literal ->
%	    write('\´),
	    fail
	; \+ Head = Literal ->
	    true
	;   fail_if_identical(Head,Literal)
	),
	add_regularity_constraints(Tail,Literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate fail_if_identical/2().

\PL*/
delay fail_if_identical(X,Y) if \==(X,Y).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Uncomment the following to see which prunes are applied.

\PL*/
% fail_if_identical(X,Y) :- writeln(X),fail.
% fail_if_identical(X,Y) :- write('/'),fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
