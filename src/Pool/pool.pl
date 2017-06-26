%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: pool.pl,v 1.7 1995/03/20 21:24:47 gerd Exp $
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
%%%
%%% The original Prolog Pool Prover
%%% was written by Gerd Neugebauer and Torsten Schaub
%%% TH Darmstadt, FG Intellektik
%%% Alexanderstr. 10, D-6100 Darmstadt
%%% ---------------------------------------------------------------------------
%%% Documentation: see Report
%%%   Gerd Neugebauer and Torsten Schaub:
%%%   A Pool Based Connection Calculus
%%% ---------------------------------------------------------------------------
%%% Adapted for ProTop by Gerd Neugebauer
%%%

:- module_interface(pool). /*%-----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.7 $}

This module implements the pool calculus in the framework of \ProTop.

\PL*/
:- export pool/0. 
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(pool).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The module has to be identified for \ProTop.

\PL*/
info(prover,
     "$Revision: 1.7 $",
     "The prover pool is an interpreting connection method theorem prover.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First of all the required modules are loaded.

\PL*/
:-	lib(lists).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(log),
	lib(time),
	lib(options),
	lib(message),
	lib(literal),
	lib(matrix).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We turn on the occurs check for this module.

\PL*/
:-	set_flag(occur_check,on).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	define_option 'pool:verbose' = on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate pool/0().

This is the main predicate in this module. \ProTop{} will call this predicate
when the option |prover| is set to pool. All informations to control the
behaviour of this predicate are communicated via the options or the matrix
modules.

The kernel is rather simple, but the management routines makes it somewhat
arkward.

\PL*/
pool :-
	is_option(log_file,Log),
	is_option(search,Search),
	( Search = iterative_deepening ->
	    A = 1, B = 1, C = 1
	; Search = iterative_deepening(A) ->
	    B = 1, C = 1 
	; Search = iterative_deepening(A,B,C) ->
	    true
	;   err("Pool: Search strategy not supported.",
		"using iterative_deepening(1,1,1) instead."),
	    A = 1, B = 1, C = 1
	),
	reset_time,
	(   set_depth_bound(A,B,C,Depth),
	    (is_option(pool:verbose) -> writeln(depth=Depth) ; true),
	    'GoalClause'(GI),
	    'Clause'(GI,Body),
	    solve_pool([hook([],Body)],Depth,Proof),

	    alarm(0),
	    statistics(runtime,[_,Time]),
	    printf("Runtime %d ms%n", [Time]),
	    ( Log == [] ->
		true
	    ;	open(Log,append,Stream),
		printf(Stream, "runtime(%w).\n", [Time]),
		close(Stream)
	    ),
	    
	    printf("%Dw\n",[proof(GI,Proof)])
	;
	    msg("No more solutions")
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate solve_pool/3(Pool,Depth,Proof).

This predicate tries to solve all hoos of a given pool within the given
depth. Upon success the proof term |Proof| is returned.

\PL*/
solve_pool([],_,[]).
solve_pool(List,Depth,[Proof1|Proof]) :-
	functor(List,'.',2),
	( Depth > 0 ->
	    Depth1 is Depth - 1
	;   setval(depth_reached,1),
	    fail
	),
	select_hook(List,hook(Path,Goals),Rest),
	solve_hook(Goals,Path,Proof1,NewPool-Rest),
	solve_pool(NewPool,Depth1,Proof).

select_hook([A|B],A,B).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate solve_hook/4(Goals,Path,Proof,HooksDL).

\PL*/
solve_hook([],_,[],X-X) :-
	!.
solve_hook(Goals,Path,[Proof|Proofs],E-L) :-
	select_literal(Goals,literal(Goal,_),RestGoals),
	negate_literal(Goal,NotGoal),
	solve_literal(NotGoal,Path,Proof,D-L),
	solve_hook(RestGoals,Path,Proofs,E-D).

select_literal([A|B],A,B).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate solve_literal/4(Literal,Path,Proof,HooksDL).

\PL*/
solve_literal(L,Path,reduction(L),X-X) :-
	sound_member(L,Path).
solve_literal(L,Path,extension(L,Idx),[hook([L|Path],Body)|X]-X) :-
	'Contrapositive'(L,Body,Idx).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate sound_member/2(Element,List).

This is a sound	 member predicate since	 the occurs check is turned on
in this module.

\PL*/
sound_member(E,[E|_]).
sound_member(E,[_|T]) :-
	sound_member(E,T).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate set_depth_bound/4(+Start, +Factor, +Const, ?Depth).

\PL*/
set_depth_bound(Start,Factor,Const,Depth) :-
	(   Depth = Start,
	    setval(depth_reached,0)
	;
	    NewStart is Factor*Start+Const,
	    getval(depth_reached,Old),
	    setval(depth_reached,0),
	    Old =\= 0,
	    set_depth_bound(NewStart,Factor,Const,Depth)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
