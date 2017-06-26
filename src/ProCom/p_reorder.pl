%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: p_reorder.pl,v 1.5 1995/05/15 19:58:27 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of P_reorder.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1995 Gerd Neugebauer
%%% 
%%% Net: gerd@imn.th-leipzig.de
%%% 
%%%****************************************************************************

:- module_interface(p_reorder). /*%--------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.5 $}

This module provides a better interface to the sorting routine. The module is
extracted from an option and several checks are performed before applying the
quicksort algorithm.

\PL*/
:- export reorder/4.

:- begin_module(p_reorder).

:-	lib(options),
	lib(message),
	lib(qsort).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reorder/4(+Unsorted, +ModuleOption, +Predicate, ?Sorted).

\PL*/
reorder(Unsorted,Option,Pred,Sorted) :-
	is_option(Option,Module),
	( empty_option(Module) ->
	    Sorted = Unsorted
	; \+ current_module(Module) ->
	    err("*** Reorder module ",Module," is not loaded."),
	    Sorted = Unsorted
	; \+ call(current_predicate(Pred/2),Module) ->
	    err("*** Reorder predicate ",Pred,
		" does not exist in module ",Module),
	    Sorted = Unsorted
	;   qsort(Unsorted,Pred,Sorted,Module)
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
