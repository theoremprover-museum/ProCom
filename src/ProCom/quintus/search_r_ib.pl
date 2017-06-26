%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: search_r_ib.pl,v 1.1 1994/03/21 23:37:17 gerd Exp $
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
% Iterative broadening stops if the depth bound is not met.
% For this we use the library ctr. (Quintus Prolog)

:- use_module(library(ctr), [ctr_set/2,ctr_set/3]).

% Iterative broadening search requires a predicate to set
% and increment the depth bound (when backtracking).
% This is provided by the predicate set_depth_bound/1,3
% The ctr 16 is used to indicate that the depth bound has been reached.
% The ctr 17 is used to indicate that the breadth bound has been reached.

:- expand_predicate(set_depth_bound/4).

set_depth_bound(Start,Factor,Const,Bound) :-
        set_depth_bound__(Start,Factor,Const,Bound),
        ctr_set(16,0),
        ctr_set(17,0).

set_depth_bound__(StartDepth*StartWidth,Factor,Const,Depth*Width) :-
        (   Depth = StartDepth,
            set_depth_bound_2(Width,StartWidth)
        ;   ctr_is(16,1),
            NewStart is Factor*StartDepth+Const,
            set_depth_bound__(NewStart*StartWidth,Factor,Const,Depth*Width)
        ).

set_depth_bound_2(X,S) :- 
        (   X = S
        ;   ctr_is(17,1),
            S1 is S+1,
            set_depth_bound_2(X,S1)
        ).


:- expand_predicate(check_depth_bound/2).

check_depth_bound(Depth*Width,NewDepth*Width) :-
        ( Depth =< 0 -> 
            write('% Depth bound reached'),nl,
            ctr_set(16,1),fail
        ;   NewDepth is Depth-1
        ).

:- expand_predicate(show_depth/1).

show_depth(Depth) :-
	write('% Depth = '),
        write(Depth),
	nl.

%
% Random selector of list elements
%
:- use_module(library(basics),  [ member/2 ]).
:- use_module(library(random),  [ random_permutation/2 ]).

:- expand_predicate(choose_step/4).

choose_step(Element,List,_*Width,_) :-
        truncate(List,Width,ShortList),
        random_permutation(ShortList,PermutedList),
        member(Element,PermutedList).

truncate([],_,[]).
truncate([H|T],N,Short) :-
        ( N < 1 ->
            ctr_set(17,1),
            Short = []
        ;   Short = [H|ST],
            N1 is  N - 1,
            truncate(T,N1,ST)
        ).
