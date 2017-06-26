%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: path-linear.pl,v 1.3 1995/01/11 09:15:45 gerd Exp $
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

This library provides path handling routines. It uses the date structure of a
linear list to store the literals.

\Predicate empty_path/1(?EmptyPath).

This predicate unifies its argument with the empty path.

\PL*/
:- expand_predicate(empty_path/1).
empty_path([]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_on_path/3(Literal,Path,NewPath).

\PL*/
:- expand_predicate(put_on_path/3).
put_on_path(Literal,Path,[Literal+_|Path]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_on_path/4(Literal,Info,Path,NewPath).

\PL*/
:- expand_predicate(put_on_path/4).
put_on_path(Literal,Info,Path,[Literal+Info|Path]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_on_path/2(Literal,Path).

\PL*/
:- expand_predicate(is_on_path/2).
is_on_path(Literal,Path) :-
	sound_member(Literal+_,Path).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_on_path/3(Literal,Info,Path).

\PL*/
:- expand_predicate(is_on_path/3).
is_on_path(Literal,Info,Path) :-
	sound_member(Literal+Info,Path).
:- require_predicate(sound_member/2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_identical_on_path/2(Literal,Path).

\PL*/
:- expand_predicate(is_identical_on_path/2).
is_identical_on_path(Literal,Path) :-
	is_identical_on_path_member(Path,Literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_identical_on_path_member/2(List,Element).

\PL*/
is_identical_on_path_member([Head+_|Tail],Element) :-
	(   Element == Head -> true
	;   is_identical_on_path_member(Tail,Element)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
