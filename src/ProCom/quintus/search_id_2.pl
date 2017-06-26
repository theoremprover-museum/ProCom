%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: search_id_2.pl,v 1.1 1994/03/21 23:37:17 gerd Exp $
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

:- use_module(library(ctr),
              [ctr_set/2,ctr_set/3,ctr_is/2,ctr_inc/2,ctr_dec/2]).

% Iterative deepening search requires a predicate to set
% and increment the depth bound (when backtracking).
% This is provided by the predicate set_depth_bound/1,3
% The ctr 16 is used to indicate that the depth bound has been reached.
% The ctr 1 is used as a global counter for the current depth

set_depth_bound(Start,Factor,Const,Depth) :-
        (   Depth = Start,
            ctr_set(1,0),
            ctr_set(16,0),
            ctr_set(17,Depth)
        ;
            NewStart is Factor*Start+Const,
            ctr_set(16,0,Old),Old =\= 0,
            set_depth_bound(NewStart,Factor,Const,Depth)
        ).

check_depth_bound(Depth,Depth) :-
        ctr_is(1,X),
        ( X > Depth -> 
            ctr_set(16,1),fail
        ;   
            ( ctr_inc(1,1)
            ; ctr_dec(1,1), fail
            )
        ).

show_depth(Depth) :-
	write('% Depth = '),
        write(Depth),
	nl.

% Essentially choose_step/3 is member/2.

choose_step(X,[X|_],_,_).
choose_step(X,[_|T],_,_) :- choose_step(X,T,[],[]).
