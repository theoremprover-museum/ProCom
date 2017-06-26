%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: time.pl,v 1.7 1994/12/12 17:07:16 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.7 $}

This module provides predicates to measure the time within Prolog.

\Predicate set_time/0().

The \eclipse{} Prolog predicate statistics/2 is used to get the real runtime.

\PL*/
:- expand_predicate(set_time/0).
set_time :- 
	statistics(runtime,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_print_time/1(+File).

\PL*/
:- expand_predicate(reset_print_time/1).
reset_print_time(LogFile) :-
	statistics(runtime,[_,TIME]),
	printf("Runtime %d ms%n",[TIME]),
	( LogFile == [] ->
	    true
	;
	    open(LogFile,append,Stream),
	    printf(Stream,"runtime(%w).\n",[TIME]),
	    close(Stream),
	    !
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate timeout_handler/0().

Predicate to be called when a timeout occurs.

\PL*/
timeout_handler :-
	writeln("*** Timeout"),
	abort.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate set_timeout/1().


\PL*/
:- expand_predicate(set_timeout/1).
set_timeout(Limit) :-
	( (integer(Limit),Limit>0) ->
	    set_interrupt_handler(alrm,timeout_handler/0),
	    alarm(Limit)
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_timeout/0().


\PL*/
:- expand_predicate(reset_timeout/0).
reset_timeout :-
	alarm(0).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
