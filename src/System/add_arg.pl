%%*****************************************************************************
%% $Id: add_arg.pl,v 1.7 1995/02/13 20:05:14 gerd Exp $
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

:- module_interface(add_arg). /*%----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.7 $}

This module provides routines which deal with adding arguments to a term or
unifying the arguments of two terms.

When I designed the |add_args| predicates I have tried to find out which
coding is more efficient. Thus I have tried a version using the functor/arg
technique and one using the univ technique. To my surprise it turned out that
the univ technique was about 60\% faster than the functor/arg variant. (Tested
with \eclipse{} 3.4.4, 10000 loops of |add_args/3| on sample data).

\PL*/
:- export unify_args/3,
	  unify_args/4,
	  add_args/3,
	  add_args/4,
	  add_args/5,
	  add_args/6,
	  add_args/7,
	  add_args/8.
:- begin_module(add_arg).

:- lib(lists).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_args/3(+Term, ?NewTerm, ?Arg1).

The term |NewTerm| is constructed from |Term| by adding an additional argument
|Arg1|. 

\PL*/
add_args(Term,NewTerm,Arg1) :-
	Term =.. List,
	append(List,[Arg1],NewList),
	NewTerm =.. NewList.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_args/4(+Term, ?NewTerm, ?Arg1, ?Arg2).

The term |NewTerm| is constructed from |Term| by adding the additional
arguments |Arg1| and |Arg2| to |Term|. 

\PL*/
add_args(Term,NewTerm,Arg1,Arg2) :-
	Term =.. List,
	append(List,[Arg1,Arg2],NewList),
	NewTerm =.. NewList.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_args/5(+Term, ?NewTerm, ?Arg1, ?Arg2, ?Arg3).

The term |NewTerm| is constructed from |Term| by adding the additional
arguments |Arg1|, |Arg2|, and |Arg3| to |Term|. 

\PL*/
add_args(Term,NewTerm,Arg1,Arg2,Arg3) :-
	Term =.. List,
	append(List,[Arg1,Arg2,Arg3],NewList),
	NewTerm =.. NewList.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_args/6(+Term, ?NewTerm, ?Arg1, ?Arg2, ?Arg3, ?Arg4).

The term |NewTerm| is constructed from |Term| by adding the additional
arguments |Arg1|, |Arg2|, |Arg3|, and |Arg4| to |Term|. 

\PL*/
add_args(Term,NewTerm,Arg1,Arg2,Arg3,Arg4) :-
	Term =.. List,
	append(List,[Arg1,Arg2,Arg3,Arg4],NewList),
	NewTerm =.. NewList.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_args/7(+Term, ?NewTerm, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5).

The term |NewTerm| is constructed from |Term| by adding the additional
arguments |Arg1|, |Arg2|, |Arg3|,|Arg4|, and |Arg5| to |Term|. 

\PL*/
add_args(Term,NewTerm,Arg1,Arg2,Arg3,Arg4,Arg5) :-
	Term =.. List,
	append(List,[Arg1,Arg2,Arg3,Arg4,Arg5],NewList),
	NewTerm =.. NewList.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate unify_args/3(+Term1, +Term2, +Length).

Unify the arguments 1 to |Length| of the terms |Term1| and |Term2|.

\PL*/
unify_args(Term1,Term2,Lenth) :-
	( Lenth =< 0 ->
	    true
	;   arg(Lenth,Term1,Arg),
	    arg(Lenth,Term2,Arg),
	    Lenth1 is Lenth-1,
	    unify_args(Term1,Term2,Lenth1)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate unify_args/4(+Term, +NewTerm, +To, +Omit).

Unify all arguments of |Term| with all arguments of |NewTerm| where the
argument number is in the range from 1 to |To|. The argument with the index
|Omit| is not unified.

\PL*/
unify_args(Term,NewTerm,To,Omit) :-
	( To < 1 -> true
	;   ( To = Omit -> true
	    ;	 arg(To,Term,Arg),
		 arg(To,NewTerm,Arg)
	    ),
	    To1 is To - 1,
	    unify_args(Term,NewTerm,To1,Omit)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped unify_args/3,
	   unify_args/4,
	   add_args/3,
	   add_args/4,
	   add_args/5,
	   add_args/6,
	   add_args/7,
	   add_args/8.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
