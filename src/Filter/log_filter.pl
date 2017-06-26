%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: log_filter.pl,v 1.1 1995/05/15 19:58:27 gerd Exp $
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

:- module_interface(log_filter). /*%-------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.1 $}


\PL*/
:- export log_filter/2.
:- begin_module(log_filter).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Our identification as a filter for \ProTop.

\PL*/
info(filter,
     "$Revision: 1.1 $",
     "Log filter to transfer information into the log file.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We need some modules from the system.

\PL*/
:-	lib(log),
	lib(op_def),
	lib(options).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate log_filter/2(+Stream, +OutStream).

\PL*/
log_filter(Stream,OutStream) :-
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	; Term = (#log(Item)) ->
	    log_put(Item), 
	    fail
	;   writeclause(OutStream,Term),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
