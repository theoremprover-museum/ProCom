%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: debugger.pl,v 1.2 1995/02/13 20:05:14 gerd Exp $
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
% Tracer for the compiled prove procedure.

:- dynamic 'Debugger Spypoint'/1.

:- expand_predicate('Debugger'/4).

'Debugger'(Literal,Candidates,Depth,Path) :-
        ( 'Debugger Spypoint'(Literal) ->
            format('[~w]: ~w ~n',[Depth,Literal])
        ;
            true
        ).

'Trace'   :- assert('Debugger Spypoint'(_)).
'Notrace' :- retractall('Debugger Spypoint'(_)).

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

'Unspy'(all) :- retractall('Debugger Spypoint'(_)).
'Unspy'([]).
'Unspy'([H|T]) :- 
        'Unspy'(H),
        'Unspy'(T).
'Unspy'(F/A) :- 
        functor(P,F,A),
        retractall('Debugger Spypoint'(P)).
