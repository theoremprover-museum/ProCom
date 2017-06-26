%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: time.pl,v 1.3 1994/12/12 17:07:16 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}

This module provides predicates to measure the time within Prolog.

\Predicate set_time/0().

The Quintus Prolog predicate statistics/2 is used to get the real runtime.

\PL*/
:- expand_predicate(set_time/0).
set_time :-
	statistics(runtime,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_print_time/1(+File).

\PL*/
:- expand_predicate(reset_print_time/1).
reset_print_time(_) :-
	statistics(runtime,[_,TIME]),
	format('Runtime ~d ms~n',[TIME]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate set_timeout/1().

I don't know how to do this in Quintus. Thus I do nothing.

\PL*/
:- expand_predicate(set_timeout/1).
set_timeout(_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_timeout/0().

I don't know how to do this in Quintus. Thus I do nothing.

\PL*/
:- expand_predicate(reset_timeout/0).
reset_timeout.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
