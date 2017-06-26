%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: search_iw.pl,v 1.2 1994/12/02 08:52:21 gerd Exp $
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
% 
% 
% 
% 
% 
% 

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

:- expand_predicate(check_depth_bound/4).

check_depth_bound(Depth,NewDepth,Alternative,_) :-
        ( Depth =< 0 -> 
            setval(depthq,1),
	    fail
	; (Alternative<<2) > Depth ->
            setval(depthq,1),
	    fail
        ;   NewDepth is Depth-1
        ).

:- expand_predicate(show_depth/1).

show_depth(Depth) :-
	printf("%% Depth = %w%n",[Depth]),
        flush(output).

:- expand_predicate(choose_step/4).

choose_step(Step,Cand,_,_) :- member(Step,Cand).


