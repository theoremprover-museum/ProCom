%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: search_ib.pl,v 1.2 1994/12/02 08:52:21 gerd Exp $
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
%% Iterative broadening according to Ginsberg & Harvey (AAAI 90)
%
% Iterative broadening stops if the depth bound is not met.
%
% Iterative broadening search requires a predicate to set
% and increment the depth bound (when backtracking).
% This is provided by the predicate set_depth_bound/1,3
% The ctr 16 is used to indicate that the depth bound has been reached.
% The ctr 17 is used to indicate that the breadth bound has been reached.

set_depth_bound(Start,Factor,Const,Bound) :-
        set_depth_bound__(Start,Factor,Const,Bound),
        setval(depthq,0),
        setval(breadthq,0).

set_depth_bound__(StartDepth*StartWidth,Factor,Const,Depth*Width) :-
        (   Depth = StartDepth,
            set_depth_bound_2(Width,StartWidth)
        ;   getval(depthq,1),
            NewStart is Factor*StartDepth+Const,
            set_depth_bound__(NewStart*StartWidth,Factor,Const,Depth*Width)
        ),
	setval(current_depth,Depth).

set_depth_bound_2(X,S) :- 
        (   X = S
        ;   getval(breadthq,1),
            S1 is S+1,
            set_depth_bound_2(X,S1)
        ).

check_depth_bound(Depth*Width,NewDepth*Width,_) :-
        ( Depth =< 0 -> 
%            write('% Depth bound reached'),nl,
            setval(depthq,1),fail
        ;   NewDepth is Depth-1
        ).

show_depth(Depth) :-
	printf("%% Depth = %w%n",[Depth]).

choose_step(X,List,_*Width,_) :-
        truncate(List,Width,ShortList),
        member(X,ShortList).

truncate([],_,[]).
truncate([H|T],N,Short) :-
        ( N < 1 ->
            setval(breadthq,1),
%            write('% Width bound reached'),nl,
            Short = []
        ;   Short = [H|ST],
            N1 is  N - 1,
            truncate(T,N1,ST)
        ).
