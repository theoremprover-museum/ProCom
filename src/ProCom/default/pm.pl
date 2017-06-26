%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: pm.pl,v 1.2 1995/05/15 19:58:27 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1995 Gerd Neugebauer
%%% 
%%% Net: gerd@imn.th-leipzig.de
%%% 
%%%****************************************************************************
% Some goodies for equality.
%----------------------------
% paramodulate/4 searches a subterm in Term
% If it unifies with LHS then it is replaced  with RHS
% and the new term NewTerm is returned.


:- expand_predicate(paramodulate/4).
paramodulate(LHS,RHS,Term,NewTerm) :-
	(   paramodulate_once(LHS,RHS,Term,NewTerm)
	;   paramodulate_once(RHS,LHS,Term,NewTerm)
	).

:- expand_predicate(paramodulate_once/4).

paramodulate_once(LHS,RHS,Term,NewTerm) :-
        nonvar_subterm(Term,NewTerm,Var,SubTerm),
        unify(SubTerm,LHS),
        unify(Var,RHS).
        
nonvar_subterm(Term,NewTerm,Var,SubTerm) :-
        (   var(Term) -> fail
        ;
            NewTerm = Var,
            SubTerm = Term
        ;
            functor(Term,Name,Arity),
            functor(NewTerm,Name,Arity),
            between(1,Arity,X),
            unify_args(Term,NewTerm,Arity,X),
            arg(X,Term,ST),
            arg(X,NewTerm,NST),
            nonvar_subterm(ST,NST,Var,SubTerm)
        ).

unify_args(Term,NewTerm,To,Omit) :-
        ( To < 1    -> true
        ;   ( To = Omit -> true
            ;    arg(To,Term,Arg),
                 arg(To,NewTerm,Arg)
            ),
            To1 is To - 1,
            unify_args(Term,NewTerm,To1,Omit)
        ).

between(From,To,Number) :- 
        From =< To,
        (   Number = From
        ;   From1 is From+1,
            between(From1,To,Number)
        ).
