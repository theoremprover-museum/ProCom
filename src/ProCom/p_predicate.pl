%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p_predicate.pl,v 1.10 1995/03/06 23:04:17 gerd Exp $
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

:- module_interface(p_predicate). /*
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.10 $}

This section contains predicates of general interest throughout the whole
compiler. Predicates to manipulate literals and functors are provided by this
module.

\PL*/
:- export negate_functor/2,
	  is_negative/1,
	  make_functor/3,
	  make_functor_from_template/4,
	  make_template/3,
	  make_internal_rep/2,
	  make_literal/8,
	  translate_head/3.
:- begin_module(p_predicate).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(options),
	lib(literal),
	lib(eq_member),
	lib(p_info),
	lib(linker).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate negate_functor/2(+Functor, ?NegFunctor).

To build a negated funtor we simply prepend a |~| to the name if it isn't
there already. Otherwise the existing |~| is discarded.

\PL*/
negate_functor(Functor,NegFunctor) :-
	( name(Functor,[0'~|NegFunctorName]) ->
	    name(NegFunctor,NegFunctorName)
	;   concat_atom(["~",Functor],NegFunctor)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_negative/1(+Template).

Check if the functor of |Template| is negative, i.e.\ if it has a
leading tilde |~|.

\PL*/
is_negative(Template) :-
	functor(Template,F,_),
	name(F,[0'~|_]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
make_pos_functor(Functor,Arity,NewFunctor) :-
	concat_atom([Functor,'/',Arity],NewFunctor).
make_neg_functor(Functor,Arity,NewFunctor) :-
	concat_atom(['~',Functor,'/',Arity],NewFunctor).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_functor/3(+Functor, +Arity, ?NewFunctor).

\PL*/
make_functor(Functor,Arity,NewFunctor) :-
	( Functor = -Fct ->
	    concat_atom(['~',Fct,'/',Arity],NewFunctor)
	;   concat_atom([Functor,'/',Arity],NewFunctor)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_functor_from_template/4(+Template, ?Functor,
					?Arity, ?InnerTemplate).

\PL*/
make_functor_from_template(Template,Functor,Arity,InnerTemplate) :-
	nonvar(Template),
	merge_literal_signs(Template,Temp),
	( Temp = (++InnerTemplate) ->
	    functor(InnerTemplate,F,Arity),
	    make_pos_functor(F,Arity,Functor)
	; Temp = (--InnerTemplate) ->
	    functor(InnerTemplate,F,Arity),
	    make_neg_functor(F,Arity,Functor)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_template/3(+Functor, +Arity, ?Template).

\PL*/
make_template(-F,A,--Template) :-
	!,
	functor(Template,F,A).
make_template(F,A,++Template) :-
	functor(Template,F,A).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_internal_rep/2(+External, ?Internal).

This predicate translates a term in external representation into internal
representation. The external representation has an explicit functor for the
positive or negative sign of the literal. The internal representation has
coded the sign and the arity into the predicate name.

\begin{BoxedSample}
  ?- make_internal_rep(-p(X),Intern).
  Intern = '~p/1'(X)

  ?- make_internal_rep(++p(X),Intern).
  Intern = 'p/1'(X)
\end{BoxedSample}
\PL*/
make_internal_rep(External,Internal) :-
	merge_literal_signs(External,Normal),
	( Normal = (--Lit) ->
	    functor(Lit,Functor,Arity),
	    make_neg_functor(Functor,Arity,NewFunctor)
	; Normal = (++Lit) ->
	    functor(Lit,Functor,Arity),
	    make_pos_functor(Functor,Arity,NewFunctor)
	),
	Lit      =.. [_         |Args],
	Internal =.. [NewFunctor|Args].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_literal/8(+Template,
			  ?Candidates,
			  ?Depth,
			  ?Path,
			  ?Proof,
			  ?Unifier,
			  ?Literal,
			  +Postfix).

Construct a target predicate from a template |Template|. The arguments |Depth|
and |Path| are appended to the predicate --- if neccessary.

|Template| can be of the form $p(a_1,\ldots,a_n)$\/ or
$-p(a_1,\ldots,a_n)$. In the case of a negative literal the negation is coded
in the literal name by preceeding it with |~|.

\PL*/
make_literal(Template,
	     Candidates,
	     Depth,
	     Path,
	     Proof,
	     Unifier-UnifierOut,
	     Literal,
	     Postfix) :-
	make_functor_from_template(Template,F,_,Templ),
	( Postfix = [] ->
	    Functor = F
	;   concat_atom([F|Postfix],Functor)
	),
	Templ =.. [_|Args],
	append(Args,L2,L1),
	( ( Candidates \== no_candidates,
	    is_option('ProCom:dynamic_reordering') ) ->
	    ArgList = [Candidates|L1]
	;   ArgList = L1
	),
	( getval('ProCom:add_depth_argument',on) ->
	    ( ( nonvar(Depth), Depth = 'InOut'(A,B)) ->
		L2 = [A,B|L3]
	    ;	L2 = [Depth|L3]
	    )
	;   L2 = L3
	),
	( 'Need Path'(F) ->
	    L3 = [Path|L4]
	;   L3 = L4
	),
	( is_option('ProCom:proof') ->
	    L4 = [Proof|L5]
	;   L4 = L5
	),
	( is_option('ProCom:unifier_argument') ->
	    L5 = [Unifier,UnifierOut]
	;   L5 = []
	),
	Literal =.. [Functor|ArgList].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate translate_head/3(+Head, -NewHead, -Subgoals).

A translation of a head is required to add sound unification. For this
purpose the head is checked for multiply occuring variables. If a
variable $X$\/ is occuring the second time, one occurence is renamed to
a new variable, say $X'$. Additionally a new subgoal is generated
which unifies those two variables $X$\/ and $X'$. This unification has
to incorporate an occurs check.

\PL*/
translate_head(Head,NewHead,Subgoals) :-
	( is_option('ProCom:occurs_check') ->
	    translate_head(Head,[],true,NewHead,Subgoals,_)
	;
	    NewHead  = Head,
	    Subgoals = true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate translate_head/6(Term,VarList,Goal,NewTerm,NewGoal,NewVarList).

\PL*/
translate_head(Term,VarList,Goal,NewTerm,NewGoal,NewVarList) :-
	( var(Term) ->
	    ( eq_member(Term,VarList) ->
		( Goal = true -> NewGoal = unify(Term,NewTerm)
		;		 NewGoal = (Goal,unify(Term,NewTerm))
		),
		require_predicate(unify/2)
	    ;
		NewTerm = Term,
		NewGoal = Goal
	    ),
	    NewVarList = [NewTerm|VarList]
	;   functor(Term,Name,Arity),
	    functor(NewTerm,Name,Arity),
	    translate_head(Arity,Term,VarList,Goal,NewTerm,NewGoal,NewVarList)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate translate_head/7(Pos,Term,VarList,Goal,NewTerm,NewGoal,NewVarList).

\PL*/
translate_head(Pos,Term,VarList,Goal,NewTerm,NewGoal,NewVarList) :-
	( Pos < 1 ->
	    NewGoal    = Goal,
	    NewVarList = VarList
	;
	    arg(Pos,Term,A),
	    arg(Pos,NewTerm,NewA),
	    translate_head(A,VarList,Goal,NewA,Goal1,VarList1),
	    Pos1 is Pos -1,
	    translate_head(Pos1,Term,VarList1,Goal1,NewTerm,NewGoal,NewVarList)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\EndProlog */
