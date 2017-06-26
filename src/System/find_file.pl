%%*****************************************************************************
%% $Id: find_file.pl,v 1.14 1995/03/06 23:04:17 gerd Exp $
%%*****************************************************************************
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

:- module_interface(find_file). /*%--------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.14 $}

This module provides a predicate to search for files. A search path and a list
of extensions is taken into account.


\PL*/
:- export find_file/4,
	  find_file_set/1.
:- begin_module(find_file).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate find_file/4(+ShortName, +Path, +Extensions, ?FullName).

This predicate searches for an existing file. For this purpose the directories
in |Path| are successively used as prefix. This path  is used only if the file
|ShortName|  is not already an	absolute  reference.  Absolute references  are
characterized by the  leading  character |~|,	i.e.  a	 reference to  a  home
directory,   or the  leading character	 |/|.  In  addition  the extensions in
|Extensions| are appended.

This  predicate is re-satisfiable, i.e.	  upon backtracking all existing files
will be found.

\begin{BoxedSample}
 find\_file(".login",[".","~"],["","~"],File).
\end{BoxedSample}

searches for the file |.login| in the current directory (|.|)  and in the home
directory (|~|). Possibly a tilde |~| is appended to find a file |File|.

\PL*/
find_file(Name,_,Extensions,FullName) :-
	(   atom(Name),
	    name(Name,[C|_]),
	    (	C = 0'~
	    ;	C = 0'/
	    )
	;   string(Name),
	    (	substring(Name,"~",1)
	    ;	substring(Name,"/",1)
	    )
	),
	!,
	member(Ext,Extensions),
	concat_atom([Name,Ext],FullName),
	( verbose -> printf(stderr,"Trying %w\n",[FullName]) ; true ),
	exists(FullName).
find_file(Name,Path,Extensions,FullName) :-
	(   atom(Name)
	;   string(Name)
	),
	member(Dir,Path),
	member(Ext,Extensions),
	concat_atom([Dir,'/',Name,Ext],FullName),
	( verbose -> printf(stderr,"Trying %w\n",[FullName]) ; true ),
	exists(FullName).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic verbose/0.
:- retract_all(verbose).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
find_file_set(quiet) :-
	retract_all(verbose).
find_file_set(verbose) :-
	( verbose ->
	    true
	;   assert(verbose)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Maybe means to search in subdirectories will be added in the future.

\PL*/
:- skipped find_file/4.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
