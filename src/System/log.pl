%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: log.pl,v 1.10 1995/05/15 19:58:27 gerd Exp $
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

:- module_interface(log). /*%--------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.10 $}

\PL*/
:- export log_reset_time_and_print/2,
	  log_reset_time_and_print/3,
	  open_log_stream/1,
	  log_put/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(log).

:-	lib(time),
	lib(options).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate log_reset_time_and_print/2(+Pre, +Post).

\PL*/
log_reset_time_and_print(Pre,Post) :-
	log_reset_time_and_print(Pre,Post,output).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate log_reset_time_and_print3/(+Pre, +Post, +Stream).

\PL*/
log_reset_time_and_print(Pre,Post,Stream) :-
	reset_time(Time),
	time_to_string(Time,String),
	printf(Stream,"%w %w %w",[Pre,String,Post]),
	is_option(log_file,LogFile),
	( empty_option(LogFile) ->
	    true
	;   open(LogFile,append,Stream),
	    printf(Stream,"time(%q,%w).\n",[Pre,Time]),
	    close(Stream),
	    !
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate log_put/1(+Term).

\PL*/
log_put(Term) :-
	is_option(log_file,LogFile),
	( empty_option(LogFile) ->
	    true
	;   open(LogFile,append,Stream),
	    printf(Stream,"%QDMw.\n",[Term]),
	    close(Stream)
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate open_log_stream1/(-Stream).

\PL*/	).
open_log_stream(Stream) :-
	is_option(log_file,ReportFile),
	\+ empty_option(ReportFile),
	open(ReportFile,append,Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped log_reset_time_and_print/2,
	   log_reset_time_and_print/3,
	   log_put/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
