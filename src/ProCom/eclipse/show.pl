%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: show.pl,v 1.9 1995/01/11 09:15:45 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.9 $}



\Predicate list_bindings/3(GoalIndex, GoalClause, Bindings).

This predicate is called after a solution has been found. It is meant to
present the solution.

\PL*/
:- expand_predicate(list_bindings/3).
list_bindings(Index,Goal,Vars) :-
	printf("%% Goal clause: %w\n%%\n%% ?-\t",Index),
	list_goal(Goal),
	writeln(".\n"),
	list_bindings(Vars).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate list_bindings/1(ListOfBindings).

To list variable bindings pairs of the form {\em Variable |=| Value} are
compiled in, where {\em Variable}\/ is a constant denoting a variable and {\em
  Value}\/ is it's value.

\PL*/
list_bindings([]).
list_bindings([H|T]) :- 
	printf("%DMw%n",[H]),
	list_bindings(T).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate list_goal/1().

\PL*/
list_goal(G) :-
	( G=(G1,G2) ->
	    list_goal(G1),
	    write(",\n%\t"),
	    list_goal(G2)
	;   printf("%DMw",[G])
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate no_more_solutions/0().

This predicate is called when the search tree has been exhausted and no more
solutions can be found.

\PL*/
:- expand_predicate(no_more_solutions/0).
no_more_solutions :- 
	writeln(" No (more) solutions").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate no_goal/0().

This predicate is called when no goal clause is left.

\PL*/
:- expand_predicate(no_goal/0).
no_goal :- 
	writeln("*** No goal left."),
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
