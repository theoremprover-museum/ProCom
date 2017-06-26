%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: show.pl,v 1.2 1995/01/11 09:15:45 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.2 $}



\Predicate list_bindings/3(GoalIndex, GoalClause, Bindings).

This predicate is called after a solution has been found. It is meant to
present the solution.

\PL*/
:- expand_predicate(list_bindings/3).
list_bindings(Index,Goal,Vars) :-
	write('% Goal clause: '),
	write(Index),
	nl,
	write('%'),
	nl,
	write('% ?-	'),
	list_goal(Goal),
	write('.'),
	nl,nl,
	list_bindings(Vars).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate list_bindings/1(ListOfBindings).

To list variable bindings pairs of the form {\em Variable |=| Value} are
compiled in, where {\em Variable}\/ is a constant denoting a variable and {\em
  Value}\/ is it's value.

\PL*/
list_bindings([]).
list_bindings([H|T]) :- 
	write(H),
	nl,
	list_bindings(T).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate list_goal/1().

\PL*/
list_goal(G) :-
	( G=(G1,G2) ->
	    list_goal(G1),
	    write(','),
	    nl,
	    write('%	'),
	    list_goal(G2)
	;   write(G)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate no_more_solutions/0().

This predicate is called when the search tree has been exhausted and no more
solutions can be found.

\PL*/
:- expand_predicate(no_more_solutions/0).
no_more_solutions :- 
	write(' No (more) solutions'),
	nl.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate no_goal/0().

This predicate is called when no goal clause is left.

\PL*/
:- expand_predicate(no_goal/0).
no_goal :- 
	write('*** No goal left.'),
	nl,
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
