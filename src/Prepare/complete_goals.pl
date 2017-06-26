%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: complete_goals.pl,v 1.7 1995/03/06 23:04:17 gerd Exp $
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

:- module_interface(complete_goals). /*
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.7 $}


To ensure completeness of a theorem prover it is necessary to consider a
complete set of goal. As a special case we get the result that any set of goal
which contains either all positive or all negative clauses is complete.
According to this observation we try to add goal marks when appropriately.

The heuristic used acts as follows:
\begin{itemize}
  \item If a negative clause is already marked as goal then all negative
	clauses are marked as well.
  \item If no negative clause but a positive clause is marked as goal then all
	positive clauses are marked as goal.
  \item Otherwise all negative clauses are marked as goal.
\end{itemize}
%
The asymetry in this heuristic prefers negative clauses. This conforms to a
negative representation of the matrix.


\PL*/
:- export complete_goals/0. 
:- begin_module(complete_goals).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
info(red,"$Revision: 1.7 $","Standard goal completion startegy.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(matrix),
	lib(options),
	lib(message).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_option complete_goals	    = on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate complete_goals/0().

Perform the completion of the goal according to the heuristic described above.

\PL*/
complete_goals :-
	is_option(complete_goals,CG),
	empty_option(CG),
	!.
complete_goals :-
	findall(C,'GoalClause'(C),Goals),
	( have_negative_clause(Goals) ->
	    msg("% All negative clauses are considered as goals."),
	    findall(C,'NegativeClause'(C),Clauses)
	; have_positive_clause(Goals) ->
	    msg("% All positive clauses are considered as goals."),
	    findall(C,'PositiveClause'(C),Clauses)
	;   
	    msg("% All negative clauses are considered as goals."),
	    findall(C,'NegativeClause'(C),Clauses)
	),
	add_goal_label(Clauses).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate have_positive_clause/1(+ListOfClauses).

Check if a positive clause is given in the list |ListOfClauses| of clause
indices.

\PL*/
:- mode have_positive_clause(+).
have_positive_clause([H|T]) :-
	( 'PositiveClause'(H) ->
	    true
	;   have_positive_clause(T)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate have_negative_clause/1(+ListOfClauses).

Check if a negative clause is given in the list |ListOfClauses| of clause
indices.

\PL*/
:- mode have_negative_clause(+).
have_negative_clause([H|T]) :-
	( 'NegativeClause'(H) ->
	    true
	;   have_negative_clause(T)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped complete_goals/0. 
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
