%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p_info.pl,v 1.8 1995/03/06 23:04:17 gerd Exp $
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

:- module_interface(p_info). /*%-----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.8 $}


\PL*/
:- export 'Need Path'/1,
	  use_path/1,
	  'Need Reduction'/1,
	  use_reductions/1,
	  init_proc/1,
	  need_proc/1,
	  need_proc/2,
	  unprocessed_proc/2. 

:- begin_module(p_info).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(matrix),
	lib(literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic 'Need Path'/1,
	   'Need Reduction'/1.
:- dynamic 'Procedure'/1,
	   'Processed Procedure'/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate use_path/1(+Type).

\PL*/
use_path(no) :-
	!,
	retract_all('Need Path'(_)).
use_path(off) :-
	!,
	retract_all('Need Path'(_)).
use_path(all) :-
	!,
	retract_all('Need Path'(_)),
	assert('Need Path'(_)).
use_path(on) :-
	!,
	retract_all('Need Path'(_)),
	assert('Need Path'(_)).
use_path(reduction) :-
	!,
	retract_all('Need Path'(_)),
	assert(('Need Path'(FA) :- 'Need Reduction'(FA))).
use_path(reductions) :-
	!,
	retract_all('Need Path'(_)),
	assert(('Need Path'(FA) :- 'Need Reduction'(FA))).
use_path(All) :-
	err("*** Error use_path/1 argument mismatch: ",All).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate use_reductions/1(+Type).

\PL*/
use_reductions(no) :-
	!,
	retract_all('Need Reduction'(_)).
use_reductions(all) :-
	!,
	retract_all('Need Reduction'(_)),
	assert('Need Reduction'(_)).
use_reductions(Functor) :-
	( 'Need Reduction'(Functor) ->
	    true
	;   assert('Need Reduction'(Functor))
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



\Predicate init_proc/1().

\PL*/
init_proc(List) :-
	retract_all('Procedure'(_)),
	retract_all('Processed Procedure'(_)),
	(   member(F/A,List),
	    need_proc(F,A),
	    fail
	;   'GoalClause'(Index),
	    once('Clause'(Index,Body)),
	    member(literal(Literal,_),Body),
	    need_proc_neg(Literal),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate need_proc/1().

\PL*/
need_proc(++Lit) :-
	functor(Lit,F,A),
	need_proc(F,A).
need_proc(--Lit) :-
	functor(Lit,F,A),
	need_proc(-F,A).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
need_proc_neg(++Lit) :-
	functor(Lit,F,A),
	need_proc(-F,A).
need_proc_neg(--Lit) :-
	functor(Lit,F,A),
	need_proc(F,A).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate need_proc/2().

\PL*/
need_proc(F,A) :-
	( 'Processed Procedure'(F/A) ->
	    true
	; 'Procedure'(F/A) ->
	    true
	; assert('Procedure'(F/A))
        ).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate unprocessed_proc/2(Functor,Arity).

\PL*/
unprocessed_proc(F,A) :-
	retract('Procedure'(F/A)),
	assert('Processed Procedure'(F/A)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
