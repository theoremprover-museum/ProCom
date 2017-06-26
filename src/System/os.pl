%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: os.pl,v 1.11 1995/07/03 11:35:12 gerd Exp gerd $
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

:- module_interface(os). /*%---------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.11 $}

This module provides an interface to some routines of the operating system. In
contrast to the \eclipse{} library predicates any failure is captured and the
predicates simply fail without raising an exception.

\PL*/
:- export delete_file/1,
	  execute/1,
	  execute_ignore/1,
	  exec_and_get/2,
	  exec_and_get_all/2,
	  full_user_name/2.
:- begin_module(os).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- lib(strings).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_file/1(+File).

This predicate deletes the given file |File|. This file can either be given as
an atom or symbol or a list of such components which are concatenated to form
a single file name.

If the arguments are not properly instanciated or the file does not exists the
predicate simply fails. Otherwise it deletes the file and succeeds.

\begin{BoxedSample}
  [user 1]: delete\_file("/tmp/abc").
\end{BoxedSample}
\begin{BoxedSample}
  [user 2]: delete\_file(["/tmp/","abc"]).
\end{BoxedSample}

\PL*/
delete_file(File) :-
	(   atom(File) ;
	    string(File) ),
	File \== [],
	exists(File),
	delete(File).
delete_file([H|T]) :-
	concat_string([H|T],File),
	delete_file(File).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate execute/1(+Command).

This predicate concatenates its arguments and gives the result to the
predicate |system/1|.

\begin{BoxedSample}
  [user 1]: execute(["/bin/","ls"]).
\end{BoxedSample}
\begin{BoxedSample}
  [user 1]: execute("/bin/ls").
\end{BoxedSample}
\begin{BoxedSample}
  [user 1]: execute(ls).
\end{BoxedSample}

\PL*/
execute(List) :-
	functor(List,'.',2),
	concat_string(List,Command),
	system(Command).
execute(Command) :-
	string(Command),
	system(Command).
execute(Command) :-
	atom(Command),
	system(Command).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate execute_ignore/1(+Command).

\PL*/
execute_ignore(Command) :-
	( execute(Command) -> true ; true).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate exec_and_get/2(+Command, ?Answer).

This predicate executes the command |Command| in a subshell and unifies the
first line of response with the string |Answer|. Note that the behaviour is
undefined if the command produces infinitly many lines.

|Command| can either be an atom, a string, or a list of strings and atoms.
Note that the predicate does {\em not}\/ insert additional spaces between
elements of a list.

\begin{BoxedSample}
  ?- exec_and_get(pwd,Answer).
  
  Answer = "/u/home/gerd"
\end{BoxedSample}

\PL*/
exec_and_get(Command,Answer) :-
	( atom(Command) ->
	    C = Command
	; string(Command) ->
	    C = Command
	; functor(Command,'.',2) ->
	    concat_string(Command,C)
	),
	exec(C,[null,Stream]),
	read_string(Stream,"\n",_,Answer),
	close(Stream),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate exec_and_get_all/2(+Command, ?Answer).

This predicate executes the command |Command| in a subshell and unifies
|Answer| with the list of all lines returned. Note that the behaviour is
undefined if the command produces infinitly many lines.

|Command| can either be an atom, a string, or a list of strings and atoms.
Note that the predicate does {\em not}\/ insert additional spaces between
elements of a list.

\begin{BoxedSample}
  ?- exec_and_get_all(pwd,Answer).
  
  Answer = ["/u/home/gerd"]
\end{BoxedSample}

\PL*/
exec_and_get_all(Command,Answer) :-
	( atom(Command) ->
	    C = Command
	; string(Command) ->
	    C = Command
	; functor(Command,'.',2) ->
	    concat_string(Command,C)
	),
	exec(C,[null,Stream]),
	findall(A,
		exec_get_line(Stream,A),
		Answer),
	close(Stream),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate exec_get_line/2(+Stream, -Line).

This predicates reads a line from the stream |Stream| and unifies the result
with |Line|. The trailing newline is stripped.  In contrast to the usual
reading predicates |exec_get_line/2| is resatisfiable.

\PL*/
exec_get_line(Stream,_) :-
	at_eof(Stream),
	!,
	fail.
exec_get_line(Stream,Line) :-
	read_string(Stream,"\n",_,Line).
exec_get_line(Stream,Line) :-
	exec_get_line(Stream,Line).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate full_user_name/2(?Uid, ?Uname).

This predicate is a sample application of the predicate |exec_and_get/2|.
Given a login name |Uid| it tries to get the full user name and unify it with
|Uname|. If |Uid| is not instaniated then it is unified with the name of the
current user.

\begin{BoxedSample}
  ?- full_user_name(gerd,Name).
  
  Name = "Gerd Neugebauer"
\end{BoxedSample}

\PL*/
full_user_name(Uid,Uname) :-
	( string(Uid) ->
	    true
	; atom(Uid) ->
	    true
	; var(Uid) ->
	    getenv("USER",Uid)
	),
	exec_and_get(["finger -m -s -f ",Uid],Answer),
	\+ substring(Answer,"???",_),
	substring(Answer,10,22,Name),
	once(substring(Name,I,2,"  ")),
	L is I - 1,
	substring(Name,1,L,Uname).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped delete_file/1,
	   execute/1,
	   execute_ignore/1,
	   exec_and_get/2,
	   exec_and_get_all/2,
	   exec_get_line/2,
	   full_user_name/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
