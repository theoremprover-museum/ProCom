%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: search_df.pl,v 1.1 1994/03/21 23:37:17 gerd Exp $
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

:- expand_predicate(set_depth_bound/1).

set_depth_bound(_).

:- expand_predicate(check_depth_bound/4).

check_depth_bound(_,_,_,_).

:- expand_predicate(show_depth/1).

show_depth(_).

:- expand_predicate(choose_step/4).

choose_step(Step,Cand,_,_) :- member(Step,Cand).

:- ensure_loaded(library(lists)).
