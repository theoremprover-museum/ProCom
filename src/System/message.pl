%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: message.pl,v 1.10 1995/02/13 20:05:14 gerd Exp $
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

:- module_interface(message). /*%----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.10 $}

This module provides output routines for informative messages and error
reports.


The module {\sf \PrologFILE} exports the following predicates:
\PL*/
:- export msg/0,
	  msg/1,
	  msg/2,
	  msg/3,
	  msg/4,
	  msg/5,
	  msgs/1,
	  msg_list/1,
	  err/0,
	  err/1,
	  err/2,
	  err/3,
	  err/4,
	  err/5,
	  err_list/1,
	  err_list/2.

:- begin_module(message).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Since we want  terms of the  form |$VAR()|  to be  printed  as variable names.
Those terms  are introduced by	the numbervars	predicate. The \eclipse{} flag
|syntax_option| has to be adapted to allow this behaviour.

\PL*/
:- set_flag(syntax_option, '$VAR').
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate msg/().

Those  predicates  write the given   arguments	to the	standard output stream
followed by a newline. Up to five arguments are curently supported.

\PL*/
msg	       :- nl.
msg(A)	       :- printf("%UDw%n",[A]).
msg(A,B)       :- printf("%UDw%UDw%n",[A,B]).
msg(A,B,C)     :- printf("%UDw%UDw%UDw%n",[A,B,C]).
msg(A,B,C,D)   :- printf("%UDw%UDw%UDw%UDw%n",[A,B,C,D]).
msg(A,B,C,D,E) :- printf("%UDw%UDw%UDw%UDw%UDw%n",[A,B,C,D,E]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate msgs/1(+List).

This predicate writes the elements of	the given list |List| to the standard
output stream.

\PL*/
msgs([]).
msgs([H|T])	  :-
	printf("%UDw",[H]),
	msgs(T).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate msg_list/1(+List).

This predicate writes  the elements of	the given list	to the standard output
stream separated by newlines.

\PL*/
msg_list([]).
msg_list([H|T])	  :-
	printf("%UDw%n",[H]),
	msg_list(T).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate err/().

Ouput routines for error messages. Currently they perform the same tasks as
the |msg| predicates. {\bf This might change.}

\PL*/
err	       :- nl.
err(A)	       :- printf("%UDw%n",[A]).
err(A,B)       :- printf("%UDw%UDw%n",[A,B]).
err(A,B,C)     :- printf("%UDw%UDw%UDw%n",[A,B,C]).
err(A,B,C,D)   :- printf("%UDw%UDw%UDw%UDw%n",[A,B,C,D]).
err(A,B,C,D,E) :- printf("%UDw%UDw%UDw%UDw%UDw%n",[A,B,C,D,E]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate err_list/1(+List).

Ouput  routine for a list  of error messages. Currently	 they perform the same
tasks as the |msg_list| predicate. {\bf This might change.}

\PL*/
err_list([]) :-
	nl.
err_list([Head|Tail])	:-
	printf("%UDw",[Head]),
	err_list(Tail).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate err_list/2(+Prefix, +List).

Print a list of eror messages in lines like  in |err_list/1|. The first one is
preceeded by |Prefix|.

\PL*/
err_list(Prefix,List) :-
	printf("%UDw",[Prefix]),
	err_list(List).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped msg/0,
	  msg/1,
	  msg/2,
	  msg/3,
	  msg/4,
	  msg/5,
	  msgs/1,
	  msg_list/1,
	  err/0,
	  err/1,
	  err/2,
	  err/3,
	  err/4,
	  err/5,
	  err_list/1,
	  err_list/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
