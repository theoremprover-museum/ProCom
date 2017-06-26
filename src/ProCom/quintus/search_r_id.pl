%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: search_r_id.pl,v 1.1 1994/03/21 23:37:17 gerd Exp $
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
% Iterative deepening stops if the depth bound is not met.
% For this we use the library ctr. (Quintus Prolog)

:- use_module(library(ctr), [ctr_set/2,ctr_set/3]).

% Iterative deepening search requires a predicate to set
% and increment the depth bound (when backtracking).
% This is provided by the predicate set_depth_bound/1,3
% The ctr 16 is used to indicate that the depth bound has been reached.

set_depth_bound(Start,Factor,Const,Depth) :-
	(   Depth = Start,
	    ctr_set(16,0),
	    ctr_set(17,Depth)
	;
	    NewStart is Factor*Start+Const,
	    ctr_set(16,0,Old),Old =\= 0,
	    set_depth_bound(NewStart,Factor,Const,Depth)
	).

check_depth_bound(Depth,NewDepth) :-
	( Depth =< 0 -> 
	    ctr_set(16,1),fail
	;   NewDepth is Depth-1
        ).

:- expand_predicate(show_depth/1).

show_depth(Depth) :-
	write('% Depth = '),
        write(Depth),
	nl.

% Random selector of list elements

:- use_module(library(basics),  [ member/2 ]).
:- use_module(library(random),  [ random_permutation/2 ]).

choose_step(Element,[Element],_,_).
choose_step(Element,[A,B|T],_,_) :-
        random_permutation([A,B|T],PermutedList),
        member(Element,PermutedList).
