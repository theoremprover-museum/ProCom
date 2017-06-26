%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: pm-top.pl,v 1.1 1994/12/12 17:07:16 gerd Exp $
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
% Some goodies for equality.
%----------------------------
% paramodulate/4 searches a subterm in Term at the top level.
% If it unifies with LHS then it is replaced  with RHS
% and the new term NewTerm is returned.

:- expand_predicate(paramodulate1/4).

:- require_predicate(unify/2).

paramodulate1(LHS,RHS,Term,NewTerm) :-
	nonvar(Term),
        unify(Term,LHS),
        unify(NewTerm,RHS).

