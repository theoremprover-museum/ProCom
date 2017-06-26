%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: search_ii.pl,v 1.5 1994/12/19 22:03:38 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.5 $}



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
check_depth_bound(Depth,NewDepth,_,Width) :-
	( Depth =< Width -> 
	    setval(depthq,1),
	    fail
	;   NewDepth is Depth-Width
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_depth/1(+Depth).

\PL*/
:- expand_predicate(show_depth/1).
show_depth(Depth) :-
	printf("%% Inference bound = %w%n%b",[Depth]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate choose_step/4().

\PL*/
:- expand_predicate(choose_step/4).
choose_step(Step,Cand,_,_) :-
	member(Step,Cand).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
