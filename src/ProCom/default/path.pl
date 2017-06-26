%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: path.pl,v 1.5 1995/01/11 09:15:45 gerd Exp $
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

/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.5 $}

This library provides path handling routines. It uses the date structure of a
pair {\em Pos-Neg}\/ to store the positive and negative literals respectivly.
In addition to the literal it is possible to place arbitrary information on
the path.

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
	    Path = P-[Literal+_|N]
	;   Path = [Literal+_|P]-N
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_on_path/4(Literal,Info,Path,NewPath).

\PL*/
:- expand_predicate(put_on_path/4).
put_on_path(Literal,Info,P-N,Path) :-
	functor(Literal,F,_),
	( name(F,[126|_]) ->
	    Path = P-[Literal+Info|N]
	;   Path = [Literal+Info|P]-N
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_on_path/2(Literal,Path).

\PL*/
:- expand_predicate(is_on_path/2).
is_on_path(Literal,P-N) :-
	functor(Literal,F,_),
	( name(F,[126|_]) ->
	    sound_member(Literal+_,N)
	;   sound_member(Literal+_,P)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate is_on_path/3(Literal,Info,Path).

\PL*/
:- expand_predicate(is_on_path/3).
is_on_path(Literal,Info,P-N) :-
	functor(Literal,F,_),
	( name(F,[126|_]) ->
	    sound_member(Literal+Info,N)
	;   sound_member(Literal+Info,P)
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
is_identical_on_path_member([Head+_|Tail],Element) :-
	(   Element == Head -> true
	;   is_identical_on_path_member(Tail,Element)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
