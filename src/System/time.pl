%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: time.pl,v 1.6 1995/02/13 20:05:14 gerd Exp $
%%%============================================================================
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

:- module_interface(time). /*%-------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.6 $}

This module is for portability reasons. It provides predicates to set,
reset and print times.

The module {\sf \PrologFILE} exports the following predicates:
\PL*/
:- export reset_time/0,			%
	  reset_time/1,			%
	  time_to_string/2,		%
	  reset_time_and_print/2,	%
	  reset_time_and_print/3.	%
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(time).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_time/0().

Reset the timer to 0. If this is not possible this predicate should
store the current time for later use.
\PL*/
reset_time :- 
	statistics(runtime,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_time/1(?Time).

Reset the timer and return the number of milliseconds |Time| since the timer
has been started or reset the last time.

\PL*/
reset_time(Time) :- 
	statistics(runtime,[_,Time]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_time_and_print/2(+Pre, +Post).

Calculate the time passed since the last call to \verb|reset_time/0|
or \verb|reset_time_and_print/2|. This time is printed preceeded by
\verb|Pre| and followed by \verb|Post|.
\PL*/
reset_time_and_print(Pre,Post) :-
	reset_time_and_print(Pre,Post,output).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate reset_time_and_print/3(+Pre, +Post, +Stream).

\PL*/
reset_time_and_print(Pre,Post,Stream) :-
	reset_time(Time),
	time_to_string(Time,String),
	printf(Stream,"%w %w %w",[Pre,String,Post]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate time_to_string/2(+Time, ?String).

This predicate converts a time |Time| in milliseconds into a readable string
format and retruns the string |String|.

\PL*/
time_to_string(Time,String) :-
	Msec is Time mod 1000,
	Sec  is (Time // 1000) mod 60,
	Min  is (Time // 60000) mod 60,
	Hour is (Time // 3600000),
	( Hour > 0 -> 
	    concat_string([Hour,"h ",Min,":",Sec," ",Msec," ms"],String)
	; Min  > 0 -> 
	    concat_string([Min,":",Sec," ",Msec," ms"],String)
	; Sec  > 0 -> 
	    concat_string([Sec,"s ",Msec," ms"],String)
	;   
	    concat_string([Msec," ms"],String)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped reset_time/0,
	   reset_time/1,
	   time_to_string/2,
	   reset_time_and_print/2,
	   reset_time_and_print/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
