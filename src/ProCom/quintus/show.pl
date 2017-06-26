%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: show.pl,v 1.3 1994/05/17 15:56:04 gerd Exp $
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
% To list variable bindings pairs of the form
%       Variable = Value
% are compiled in, where Variable is a constant denoting
% a variable and Value is it's value.

list_bindings([]).
list_bindings([H|T]) :- 
        write(H),nl,
        list_bindings(T).

list_bindings(Index,Clause,Vars) :-
	list_bindings(Vars).

:- expand_predicate(list_bindings/3).

no_more_solutions :- 
	write(' No (more) solutions'),
	nl.

:- expand_predicate(no_more_solutions/0).

no_goal :- 
	write('*** No goal left.'),
	nl,
	fail.

:- expand_predicate(no_goal/0).

% To allow several solutions to be found the following predicate
% asks if the search should be continued.

:- expand_predicate(more/1).

more(Depth) :- 
        ( var(Depth) -> true
        ; format('Current depth is ~w. ',[Depth])
        ),
        write('Continue? '),
        \+ get(0'y).
