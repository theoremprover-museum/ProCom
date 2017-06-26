%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: qsort.pl,v 1.9 1995/02/13 20:05:14 gerd Exp $
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

:- module_interface(qsort). /*%------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.9 $}

This module implements the    quicksort	 algorithm which uses	an   arbitrary
comparison predicate. This comparison predicate should be called in the caller
module of  |qsort/3|. To accomplish this  |qsort/3| is declared	 as tool. I.e.
\eclipse{} provides the callers module name  in an additional fourth argument.
Thus the external usable predicate is

\Predicate qsort/3(+Unsorted, +Comparison, ?Sorted).

The use of this predicate can be seen in th following example.  The numbers
are sorted according to the built-in |>=| relation. The result is shown below.
\begin{BoxedSample}
  ?- qsort([2,1,4,3],>=,X).
  X = [4, 3, 2, 1]
\end{BoxedSample}

\PL*/
:- export qsort/4.
:- tool(qsort/3,qsort/4).
:- begin_module(qsort).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The implementation uses |append/3|. This is taken from the library |lists|.
\PL*/
:-	lib(lists).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate qsort/4(+Unsorted, +Comparison, ?Sorted, +Module).

Sort the  list	|Unsorted|  using  the predicate   |Comparison| in  the module
|Module|. The	result	is |Sorted|.  The algorithm  used   is essentially the
quicksort  algorithm. Up to three  elements are sorted directly. Otherwise the
median of the	 first three elements  is  used	  to partition the  list   and
recursivly sort the  smaller parts.    Finally the  sorted lower  part,	   the
partitioning element and the higher part are appended to form the sorted list.

The use of this predicate can be seen in th following example.
The numbers are sorted according to the |<| relation. This relation is called
in the module |user|. The result is shown below.
\begin{BoxedSample}
  ?- qsort([2,1,4,3],<,X,user).
  X = [1, 2, 3, 4]
\end{BoxedSample}

\PL*/
qsort([],_,[],_).
qsort([A],_,[A],_).
qsort([A,B],Compare,Result,Module) :-
	functor(COMPARE,Compare,2),
	arg(1,COMPARE,A),
	arg(2,COMPARE,B),
	( call(COMPARE,Module) ->
	    Result = [A,B]
	;   Result = [B,A]
	).
qsort([A,B,C],Compare,[Low,Med,High],Module) :-
	sort3(A,B,C,Compare,Module,Low,Med,High).
qsort([A,B,C,D|R],Compare,Sorted,Module) :-
	sort3(A,B,C,Compare,Module,Low,Med,High),
	qpartition([D|R],Compare,Module,Med,Lowers,Highers),
	qsort([Low|Lowers],Compare,L,Module),
	qsort([High|Highers],Compare,H,Module),
	append(L,[Med|H],Sorted),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate qpartition/6(+List, +Comparison, +Module, +Median,
			?LowerList, ?HigherList).

This  predicate	 takes	a list	|List| and  a  reference element  |Median| and
decomposes the list  into |LowerList| and  |HigherList|. This decomposition is
done  according to  the	  binary comparison predicate  |Comparison|  in module
|Module|. Elements $E$\/ of  |List| for which $|Comparison|(E,|Median|)$\/  is
true are stored in |LowerList|. The other elements are stored in |HigherList|.

\PL*/
qpartition([],_,_,_,[],[]).
qpartition([E|Rest],Compare,Module,Median,Lowers,Highers) :-
	functor(COMPARE,Compare,2),
	arg(1,COMPARE,E),
	arg(2,COMPARE,Median),
	( call(COMPARE,Module) ->
	    Lowers  = [E|Low],
	    Highers = High
	;   Lowers  = Low,
	    Highers = [E|High]
	),
	qpartition(Rest,Compare,Module,Median,Low,High).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate sort3/8(A, B, C, Comparison, Module, Low, Med, High).

Sort three  elements |A|, |B|, and |C|	using the binary  comparison predicate
|Comparison|  in module |Module|. The result  is retruned in |Low|, |Med|, and
|High|.

The predicate  uses extensive case analysis. Each  case is considered  and the
appropriate result is returned.

\PL*/
sort3(A,B,C,Compare,Module,Low,Med,High) :-
	functor(COMPARE_AB,Compare,2),
	arg(1,COMPARE_AB,A),
	arg(2,COMPARE_AB,B),
	( call(COMPARE_AB,Module) ->
	    functor(COMPARE_BC,Compare,2),
	    arg(1,COMPARE_BC,B),
	    arg(2,COMPARE_BC,C),
	    ( call(COMPARE_BC,Module) ->
		Low  = A,
		Med  = B,
		High = C
	    ;
		High = B,
		functor(COMPARE_AC,Compare,2),
		arg(1,COMPARE_AC,A),
		arg(2,COMPARE_AC,C),
		( call(COMPARE_AC,Module) ->
		    Low = A,
		    Med = C
		;   Low = C,
		    Med = A
		)
	    )
	;
	    functor(COMPARE_AC,Compare,2),
	    arg(1,COMPARE_AC,A),
	    arg(2,COMPARE_AC,C),
	    ( call(COMPARE_AC,Module) ->
		Low  = B,
		Med  = A,
		High = C
	    ;
		High = A,
		functor(COMPARE_BC,Compare,2),
		arg(1,COMPARE_BC,B),
		arg(2,COMPARE_BC,C),
		( call(COMPARE_BC,Module) ->
		    Low = B,
		    Med = C
		;
		    Low = C,
		    Med = B
		)
	    )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped qsort/4.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
