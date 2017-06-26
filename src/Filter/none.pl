%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: none.pl,v 1.6 1995/02/06 08:57:39 gerd Exp $
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

:- module_interface(none). /*%-------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.6 $}

The filter {\tt none} is a very simple filter which does simply nothing. It is
mainly provided as a basis for your own development of a filter.

\PL*/
:- export none/2.
:- begin_module(none).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Our identification as a filter for \ProTop.

\PL*/
info(filter,"$Revision: 1.6 $","Empty filter").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We need some definitions from the system.

\PL*/
:-	lib(op_def).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate none/2(+Stream, +OutStream).

This is an empty filter. No action is performed except that terms are
transfered from the input stream |Stream| to the output stream
|OutStream|.

\PL*/
none(Stream,OutStream) :-
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	;   writeclause(OutStream,Term),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
