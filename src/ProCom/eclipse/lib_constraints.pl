%%*****************************************************************************
%% $Id: lib_constraints.pl,v 1.1 1994/12/08 13:38:16 gerd Exp $
%%*****************************************************************************
%% Author: Gerd Neugebauer
%%=============================================================================


:- expand_predicate(enumerate_cst/2).

enumerate_cst(List, _) :-
	call(enum_cst(List), constraints).

:- erase_module(constraint_theory).

:- use_module(constraints).
