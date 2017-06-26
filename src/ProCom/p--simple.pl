%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p--simple.pl,v 1.5 1995/01/27 13:45:38 gerd Exp $
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

:- module('p--simple'). /*%----------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.5 $}

This module implements a simple reordering strategy.

\PL*/
:- export compare_extensions/4.

:-	lib(matrix).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate compare_extensions/3().

\PL*/
compare_extensions(_,A-_,_,B-_) :-
	'ClauseLength'(A) < 'ClauseLength'(B).
compare_extensions(LitA,A-_,LitB,B-_) :-
	'ClauseLength'(A) = 'ClauseLength'(B),
	term_variables(LitA,VA),
	term_variables(LitB,VB),
	length(VA) =< length(VB).


compare_prolog_clauses((HA:-A),(HB:-B)) :-
	conjunction_length(A,LA),
	conjunction_length(B,LB),
	( LA < LB ->
	    true
	; LA > LB ->
	    fail
	; instance(HA,HB) ->
	    true
	; instance(HB,HA) ->
	    fail
	;
	    term_vars(HA,VA),
	    term_vars(HB,VB),
	    length(VA) < length(VB)
	).


term_vars(Term,Vars) :-
	term_vars(Term,[],Vars).

term_vars(Term,Vars,NewVars) :-
	( var(Term) ->
	    ( occurs(Term,Vars) ->
		NewVars = Vars
	    ;	NewVars = [Term|Vars]
	    )
	; atomic(Term) ->
	    NewVars = Vars
	;   functor(Term,_,Arity),
	    term_vars(Term,Arity,Vars,NewVars)
	).
term_vars(Term,Arg,Vars,NewVars) :-
	( Arg < 1 ->
	    NewVars = Vars
	;   arg(Arg,Term,ArgTerm),
	    term_vars(ArgTerm,Vars,MoreVars),
	    Arg1 is Arg-1,
	    term_vars(Term,Arg1,MoreVars,NewVars)
	).


conjunction_length(C,L) :-
	( C = (A,B) ->
	    conjunction_length(A,LA),
	    conjunction_length(B,LB),
	    L is LA + LB
	;   L = 1
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
