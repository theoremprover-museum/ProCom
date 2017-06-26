%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: debugger.pl,v 1.4 1995/01/11 09:15:45 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.4 $}

This library provides an debugger for the compiled prove procedure.

\Predicate Debugger/4(Literal,Candidates,Depth,Path).

This is the main predicate provided by this library.

\PL*/
:- expand_predicate('Debugger'/4).
'Debugger'(Literal,Candidates,Depth,Path) :-
	'Debugger Spypoint'(Literal),
	printf("[%w]: %w%n",[Depth,Literal]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic 'Debugger Spypoint'/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Trace/0().

\PL*/
'Trace'	  :- assert('Debugger Spypoint'(_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Notrace/0().

\PL*/
'Notrace' :- retractall('Debugger Spypoint'(_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Spy/1().

\PL*/
'Spy'([]).
'Spy'([H|T]) :- 
	'Spy'(H),
	'Spy'(T).
'Spy'(F/A) :- 
	functor(P,F,A),
	( 'Debugger Spypoint'(P) ->
	   true
	;  assert('Debugger Spypoint'(P))
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Unspy/1(+Which).

\PL*/
'Unspy'(all) :-
	retractall('Debugger Spypoint'(_)).
'Unspy'([]).
'Unspy'([H|T]) :- 
	'Unspy'(H),
	'Unspy'(T).
'Unspy'(F/A) :- 
	functor(P,F,A),
	retractall('Debugger Spypoint'(P)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
