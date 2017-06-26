%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: search_r_df.pl,v 1.1 1994/03/21 23:37:17 gerd Exp $
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
% Random selector of list elements
%
:- use_module(library(basics),  [ member/2 ]).
:- use_module(library(random),  [ random_permutation/2 ]).

choose_step(Element,[Element],_,_).
choose_step(Element,[A,B|T],_,_) :-
        random_permutation([A,B|T],PermutedList),
        member(Element,PermutedList).

:- expand_predicate(show_depth/1).

show_depth(Depth).
