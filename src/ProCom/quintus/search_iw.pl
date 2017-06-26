%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: search_iw.pl,v 1.1 1994/03/21 23:37:17 gerd Exp $
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
% Iterative widening stops if the depth bound is not met.
% For this we use the library ctr. (Quintus Prolog)

:- use_module(library(ctr), [ctr_is/2,ctr_set/2,ctr_set/3]).

% Iterative widening search requires a predicate to set
% and increment the depth bound (when backtracking).
% This is provided by the predicate set_depth_bound/1,3
% The ctr 16 is used to indicate that the depth/width bound has been reached.
% The ctr 20 is used to store the maximum depth

set_depth_bound(Start,Factor,Const,Depth) :-
        (   Depth = Start,
            ctr_set(16,0),
            ctr_set(20,Depth)
        ;
            NewStart is Factor*Start+Const,
            ctr_set(16,0,Old),Old =\= 0,
            set_depth_bound(NewStart,Factor,Const,Depth)
        ).

:- expand_predicate(check_depth_bound/4).

check_depth_bound(Depth,NewDepth,Alternative,_) :-
        ( Depth =< 0 -> 
            ctr_set(16,1),
	    fail
	; Alternative > Depth/4
            ctr_set(16,1),
	    fail
        ;   NewDepth is Depth-1
        ).

:- expand_predicate(show_depth/1).

show_depth(Depth) :-
	format('% Depth = ~w~n',Depth).

:- expand_predicate(choose_step/4).

choose_step(Step,Cand,_,_) :- member(Step,Cand).

