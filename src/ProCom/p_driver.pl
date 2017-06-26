%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p_driver.pl,v 1.11 1995/02/13 20:05:14 gerd Exp $
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

:- module_interface(p_driver). /*%-------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.11 $}


\PL*/
:- export make_driver/3,
	  make_driver_clauses/3.
:- begin_module(p_driver).

:-	lib(options),
	lib(add_arg),
	lib(literal),
	lib(linker),
	lib(p_info),
	lib(p_predicate).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_driver/3(+Functor, +Arity, -Clause).

\PL*/
make_driver(Functor,Arity,Clause) :- !,
	make_functor(Functor,Arity,Proc),
	( Functor = -F ->
	    functor(E,F,Arity),
	    negate_functor(F,NegF),
	    functor(PrologEntry,NegF,Arity),
	    unify_args(PrologEntry,E,Arity),
	    Entry = (++E)
	;
	    functor(PrologEntry,Functor,Arity),
	    Entry = (-- PrologEntry)
	),
	negate_literal(Entry,NegEntry),
	make_literal(NegEntry,Name,Depth,Path,Proof,Unifier,PrologHead,[]),
	Name = [_|_],
	( getval('ProCom:add_depth_argument',on) ->
	    require_predicate(check_depth_bound/4),
	    Search_Test = check_depth_bound(Depth,NewDepth,_,_)
	;   Search_Test = true
	),
	( is_option('ProCom:ancestor_pruning') ->
	    require_predicate(is_identical_on_path/2),
	    Ancestor_Test = (\+ is_identical_on_path(PrologEntry,Path))
	;   Ancestor_Test = true
	),
	( ( 'Need Path'(Proc),
	    is_option('ProCom:automatic_put_on_path') ) ->
	    PushToPath = put_on_path(PrologEntry,Path,NewPath )
	;   PushToPath = true,
	    Path = NewPath
	),
	( is_option('ProCom:trace') ->
	    CallDebugger = 'Debugger'(Entry,Name,Depth,Path)
	;   CallDebugger = true
	),
	make_literal(NegEntry,Cand,NewDepth,NewPath,Proof,Unifier,
		     NewPrologHead,[]),
	require_predicate(choose_step/4),
	Clause = (PrologHead :-
		      CallDebugger,
		      Search_Test,
		      Ancestor_Test,
		      PushToPath,
		      choose_step(Cand,Name,Depth,PrologHead),
		      NewPrologHead  ).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_driver_clauses/3(+Functor, +Arity, -Clause).

\PL*/
make_driver_clauses(Functor,Arity,Clauses-End) :- !,
	( Functor = -F ->
	    functor(E,F,Arity),
	    negate_functor(F,NegF),
	    functor(PrologEntry,NegF,Arity),
	    unify_args(PrologEntry,E,Arity),
	    NegEntry = (-- E),
	    FA = -F/Arity
	;
	    functor(PrologEntry,Functor,Arity),
	    NegEntry = (++ PrologEntry),
	    FA = Functor/Arity
	),
	( getval('ProCom:depth_pass_through',on) ->
	    Depth='InOut'(_,_)
	;   true
	),
	make_functor(Functor,Arity,Fct),
	functor(Proc,Fct,Arity),
	unify_args(Proc,PrologEntry,Arity),
	make_literal(NegEntry,
		     _,
		     Depth,
		     Path,
		     Proof,
		     Unifier,
		     PrologHead,[]),
	findall(C,
		make_cl(PrologHead, FA, Proc, Depth, Path, Proof, Unifier, C),
		CLAUSES),
	append(CLAUSES,End,Clauses).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_cl/8(Head,FunctorArity,Proc,Depth,Path,Proof,Unifier,Clause).

\PL*/
make_cl(Head,FA,Proc,Depth,Path,_,_,Clause) :-
	is_option('ProCom:trace',TRACE),
	(   TRACE == on
	;   functor(TRACE,'.',2),
	    member(literals,TRACE)
	;   functor(TRACE,'.',2),
	    member(all,TRACE)
	;   functor(TRACE,'.',2),
	    member(spy(FA),TRACE)
	),
	require_predicate('Debugger'/4),
	Clause = (Head :- 'Debugger'(Proc,_,Depth,Path), fail).
make_cl(Head,_,Proc,_,Path,_,_,Clause) :-
	is_option('ProCom:ancestor_pruning'),
	Proc =.. [H|Args], 
	negate_functor(H,NegH),
	NegProc =.. [NegH|Args],
	require_predicate(is_identical_on_path/2),
	Clause = (Head :- is_identical_on_path(NegProc,Path), !, fail).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
