/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: me_para.pl,v 1.11 1995/06/23 06:35:31 gerd Exp $
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
\PL*/
:- module(me_para).
:- compile(library(capri)).
:- use_module(add_arg).

require_option(equality,[on]).

force_option('ProCom:use_path',[all]).
force_option('ProCom:use_reductions',[all]).
force_option(apply_reductions,[off]).
force_option(remove_unreached_clauses,[off]).

descriptor
	template(Pred,goal),
	template(-Pred,path).

descriptor
	name('reflexivity of equality'),
	template(--(A=B),goal),
	constructor(unify(A,B)).

descriptor
	template(Pred,goal),
	template(-Pred,extension).

descriptor
        name(paramodulation),
	template(Pred,goal),
	get(functor(Pred),_/Arity),
	call(between(1,Arity,1,X)),
	call(modify(Pred,X,A,B,Pred2)),
	template(++(LHS=RHS),[path,extension]),
	constructor( paramodulate(LHS,RHS,A,B) ),
	template(Pred2,residue).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
library_file(pm).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate modify/5().

\PL*/
modify(--Pred,Pos,A,B,--Mod) :- !,
	modify(Pred,Pos,A,B,Mod).
modify(++Pred,Pos,A,B,++Mod) :- !,
	modify(Pred,Pos,A,B,Mod).
modify(Pred,Pos,A,B,Mod) :-
	functor(Pred,F,Arity),
	functor(Mod,F,Arity),
	arg(Pos,Pred,A),
	arg(Pos,Mod,B),
	unify_args(Pred,Mod,Arity,Pos).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
