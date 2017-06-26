%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: varlist.pl,v 1.6 1995/02/13 20:05:14 gerd Exp $
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

:- module_interface(varlist). /*%----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.6 $}

This module provides a predicate to analyze a term and collect all variables.

\PL*/
:- export varlist/2,
	  varlist/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(varlist).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate varlist/2(+Term, -VariableList).

Collect a list of all variables in |Term|. The variables are returned
as {\em Name}| = Value| terms, where {\em Name} is a variable
name which is constructed similar to |numbervars/1|.

Internally predicates with richer argument lists are used.
\PL*/
varlist(Term,List) :-
	copy_term(Term,TermCopy),
	varlist(Term,TermCopy,[],List,65,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate varlist/3(Term1, Term2, ?VarList).

\PL*/
varlist(Term,TermCopy,List) :-
	copy_term(Term,TermCopy),
	varlist(Term,TermCopy,[],List,65,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate varlist/6(Term1, Term2, VarListIn, VarList, N, N1).

\PL*/
varlist(Term1,Term2,VarListIn,VarList,N,N1) :-
	var(Term2),
	!,
	VarList = [(Term2=Term1)|VarListIn],
	name(Term2,[N]),
	N1 is N+1.
varlist(_,[],VarList,VarList,N,N) :-
	!.
varlist(Term1,Term2,VarListIn,VarList,N,N1) :-
	functor(Term2,_,A),
	varlist(Term1,Term2,VarListIn,VarList,N,N1,A).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate varlist/7(Term1, Term2, InList, List, N, N2, Pos).

\PL*/
varlist(Term1,Term2,InList,List,N,N2,Pos) :-
	( Pos<1 -> 
	    InList = List,
	    N2	   = N
	;
	    arg(Pos,Term1,T1),
	    arg(Pos,Term2,T2),
	    Next is Pos-1,
	    varlist(T1,T2,InList,List2,N,N1),
	    !,
	    varlist(Term1,Term2,List2,List,N1,N2,Next)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped varlist/2,
	   varlist/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
