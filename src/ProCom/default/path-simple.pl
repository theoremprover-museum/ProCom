%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: path-simple.pl,v 1.3 1995/01/11 09:15:45 gerd Exp $
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
pair {\em Pos-Neg}\/ to store the positive and negative literals respectivly.

\Predicate empty_path/1(?EmptyPath).

This predicate unifies its argument with the empty path.

\PL*/
:- expand_predicate(empty_path/1).
empty_path([]-[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_on_path/3(Literal,Path,NewPath).

\PL*/
:- expand_predicate(put_on_path/3).
put_on_path(Literal,P-N,Path) :-
	functor(Literal,F,_),
	( name(F,[126|_]) ->
	    Path = P-[Literal|N]
	;   Path = [Literal|P]-N
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_on_path/2(Literal,Path).

\PL*/
:- expand_predicate(is_on_path/2).
is_on_path(Literal,P-N) :-
	functor(Literal,F,_),
	( name(F,[126|_]) ->
	    sound_member(Literal,N)
	;   sound_member(Literal,P)
	).
:- require_predicate(sound_member/2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_identical_on_path/2(Literal,Path).

\PL*/
:- expand_predicate(is_identical_on_path/2).
is_identical_on_path(Literal,P-N) :-
	functor(Literal,F,_),
	( name(F,[126|_]) ->
	    is_identical_on_path_member(N,Literal)
	;   is_identical_on_path_member(P,Literal)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_identical_on_path_member/2(List,Element).

\PL*/
is_identical_on_path_member([Head|Tail],Element) :-
	(   Element == Head -> true
	;   is_identical_on_path_member(Tail,Element)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
