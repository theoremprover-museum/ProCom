%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p_options.pl,v 1.11 1995/03/06 23:04:17 gerd Exp $
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

:- module_interface(p_options). /*%--------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.11 $}

\PL*/
:- export force_options/1,
	  apply_options/2,
	  require_options/1,
	  check_requirement/1.
:- begin_module(p_options).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(matrix),
	lib(options),
	lib(literal),
	lib(linker),
	lib(p_predicate),
	lib(p_info).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate require_options/1(+Module).

Test if all required options for |Module| are fulfilled. For this purpose the
predicate |require_option/2| in the module |Module| is evaluated.

\PL*/
require_options(Module) :-
	findall((Option=Values),
		(call(require_option(Option,Values),Module),
		 (functor(Values,'.',2) ->
		     true
		 ;   err("*** Type error in require_option. List expected but got ",
		 	 Values),
		     fail
		 )
		),
		SpecList),
	require_opts(SpecList).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate require_opts/1(+Requirements).

This predicate checks if a list of requirements |Requirements| is fulfilled.
The requirements are terms of the form {\it option |=| pattern}, where {\it
  option}\/ is an option and {\it pattern}\/ is a pattern to be unified with
the value of {\it option}.

This predicate succeeds once iff all requirements are fullfilled.

\PL*/
require_opts([]).
require_opts([(Opt=Cand)|T]) :-
	is_option(Opt,Value),
	member(Value,Cand),
	!,
	require_opts(T).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate check_requirement/1(+Module).

This predicate calls the required initialization predicates in the module
|Module|. For this purpose the predicate |requirement/1| in the module
|Module| is used to get the Prolog goals to be called afterwards. The number
of solutions of each predicate is limited to one.

In a failure driven loop all required Prolog goals are evaluated in turn.

\PL*/
check_requirement(Module) :-
	(   call(requirement(Pred),Module),
	    once(Pred),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate force_options/1(+Module).

Check the options for module. If an inconsistency is found modify the
options according to the first possible value.

\PL*/
force_options(Module) :-
	(   call(force_option(Name,Values),Module),
	    ( functor(Values,'.',2) ->
		is_option(Name,ActualValue),
		\+ member(ActualValue,Values),
		Values = [FirstChoice|_],
		set_option(Name=FirstChoice)
	    ;   err("Type error in force_option. List expected but got ",
		    Values)
	    ),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate apply_options/2(?Functors, +Module).

\PL*/
apply_options(FunctorList,Module) :-
	force_options(Module),

	is_option('ProCom:use_path',UsePath),
	use_path(UsePath),

	use_reductions(no),
	( setof(Functor_Arity,get_functor(Functor_Arity),FunctorList) ->
	    true
	;   FunctorList = []
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_functor/1(-FunctorArity).

In turn give the functors of all predicates stored as head of
|'Clause'/3|. This predicate is used to encapsulate free variables for
a call to |setof/3|.

As a side effect the predicate |'Need Reduction'| is constructed.

As an extreme case we have to deliver the functors occuring in goal
clauses aswell to prevent a runtime error when procedures are not
found.
\PL*/
get_functor(Functor/Arity) :-
	'Contrapositive'(Head,_,CL),
	( Head = (-- Literal) -> 
	    functor(Literal,NegFunctor,Arity),
	    Functor = -NegFunctor
	;   Head = (++ Literal),
	    functor(Literal,Functor,Arity)
	),
	( 'Connection'(CL,_) ->
	    make_functor(Functor,Arity,NewFunctor),
	    use_reductions(NewFunctor)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
As an extreme case we have to deliver the functors occuring in goal
clauses aswell to prevent a runtime error when procedures are not
found.

Note that literals considered as goals have to be negated.
\PL*/
%get_functor(Functor/Arity) :-
%	'GoalClause'(Idx),
%	'CP_by_index'(Idx,_,H,List),
%	(   Head = H
%	;   member(literal(Head,_),List)
%	),
%	( Head = (-- Literal) -> 
%	    functor(Literal,Functor,Arity)
%	; Head = (++ Literal),
%	    functor(Literal,NegFunctor,Arity),
%	    Functor = -NegFunctor
%	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
get_functor(Functor/Arity) :-
	is_option('ProCom:extra_procedures',ProcList),
	member(Functor/Arity,ProcList).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
