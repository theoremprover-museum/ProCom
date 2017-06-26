%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: search_id.pl,v 1.4 1994/12/19 22:03:38 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.4 $}

This library implements the iterative deepening search strategy.

Iterative deepening stops if the depth bound is not met.  For this purpose we
use global variables of \eclipse{} Prolog.

Iterative deepening search requires a predicate to set and increment the depth
bound (when backtracking).  This is provided by the predicates
|set_depth_bound/4| and |check_depth_bound/4|. The counter |depthq| is used to
indicate that the depth bound has been reached.


\Predicate set_depth_bound/4(+Start, +Factor, +Const, ?Depth).

\PL*/
set_depth_bound(Start,Factor,Const,Depth) :-
	(   Depth = Start,
	    setval(depthq,0)
	;
	    NewStart is Factor*Start+Const,
	    getval(depthq,Old),
	    setval(depthq,0),
	    Old =\= 0,
	    set_depth_bound(NewStart,Factor,Const,Depth)
	),
	setval(current_depth,Depth).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate check_depth_bound/4().

\PL*/
:- expand_predicate(check_depth_bound/4).
check_depth_bound(Depth,NewDepth,_,_) :-
	( Depth =< 0 -> 
	    setval(depthq,1),
	    fail
	;   NewDepth is Depth-1
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_depth/1(+Depth).

\PL*/
:- expand_predicate(show_depth/1).
show_depth(Depth) :-
	printf("%% Depth = %w%n%b",[Depth]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate choose_step/4(?Step, +Candidates, , ).

\PL*/
:- expand_predicate(choose_step/4).
choose_step(Step,Cand,_,_) :-
	member(Step,Cand).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
