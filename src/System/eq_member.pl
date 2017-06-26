%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: eq_member.pl,v 1.11 1995/04/06 12:53:14 gerd Exp $
%%%============================================================================
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

:- module_interface(eq_member). /*%-------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.11 $}

This module contains predicates to deal with identical elements of lists. E.g.
a list of variables must be treated special to avoid unification where only a
check for identity is required.

\PL*/
:- export eq_member/2,
	  eq_subtract/3,
	  eq_union/3,
	  eq_intersect/3.
:- begin_module(eq_member).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\Predicate eq_member/2(+Element,+List).

This predicate searches a list for an equal (|==|) element. It
succeeds at most once.

\begin{BoxedSample}
eq\_member(X,[a,B,c,B]).
\end{BoxedSample}
fails.

\begin{BoxedSample}
eq\_member(B,[a,B,c,B]).
\end{BoxedSample}
succeeds once.

\PL*/
eq_member(Element,[Head|Tail]) :-
	( Element == Head -> true
	;    eq_member(Element,Tail)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate eq_subtract/3(+List, +Candidates, ?Result).

This predicate takes a list |List| and deletes all elements which are identical
to an element in the list |Candidates|. The result is unified with |Result|.

\begin{BoxedSample}
eq\_subtract([A,B,C,B,D],[B,D],X).
\end{BoxedSample}
returns |X = [A, C]|.

\PL*/
eq_subtract([],_,[]).
eq_subtract([H|T],Cand,Result) :-
	( eq_member(H,Cand) ->
	    eq_subtract(T,Cand,Result)
	;   Result = [H|Rest],
	    eq_subtract(T,Cand,Rest)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate eq_union/3(+List1, +List2, ?Result).

This predicate takes two lists |List1| and |List2| and returns the union.
Elements which are in both lists are only taken once. The result is unified
with |Result|.

\begin{BoxedSample}
eq\_union([A,B,C],[B,D],X).
\end{BoxedSample}
returns |X = [A, B, C, D]|.

\PL*/
eq_union([],List2,List2).
eq_union([H|T],List2,Result) :-
	( eq_member(H,List2) ->
	    eq_union(T,List2,Result)
	;   Result = [H|R],
	    eq_union(T,List2,R)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate eq_intersect/3(+List1, +List2, ?Result).

This predicate takes two lists |List1| and |List2| and returns those elements
which are in both lists. The result is unified with |Result|.

\begin{BoxedSample}
eq\_intersect([A,B,C],[C,B,D],X).
\end{BoxedSample}
returns |X = [C, B]|.

\PL*/
eq_intersect([],_,[]).
eq_intersect([H|T],List2,Result) :-
	( eq_member(H,List2) ->
	    Result = [H|R],
	    eq_intersect(T,List2,R)
	;   eq_intersect(T,List2,Result)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped eq_member/2,
	   eq_subtract/3,
	   eq_union/3,
	   eq_intersect/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
