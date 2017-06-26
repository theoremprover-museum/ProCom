%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: nf_neg_in.pl,v 1.2 1995/04/10 19:24:50 gerd Exp $
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

:- module_interface(nf_neg_in). /*%--------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.2 $}


\PL*/
:- export neg_in/2.
:- begin_module(nf_neg_in).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First we load some libraries. Primarily this is done to get some operators
declared appropriately.

\PL*/
:- 	lib(eq_member),
	lib(nf_intern).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate neg_in/2(Formula, Result).

\PL*/
neg_in(true,true).
neg_in(false,false).
neg_in(not(F),NF) :-
	neg_in_neg(F,NF).
neg_in(predicate(P),predicate(P)).
neg_in((A and B),(NA and NB)) :-
	neg_in(A,NA),
	neg_in(B,NB).
neg_in((A or B),(NA or NB)) :-
	neg_in(A,NA),
	neg_in(B,NB).
neg_in((A implies B),(NA or NB)) :-
	neg_in(not(A),NA),
	neg_in(B,NB).
neg_in((A iff B),(NA iff NB)) :-
	neg_in(A,NA),
	neg_in(B,NB).
neg_in(exists(V,F),exists(V,NF)) :-
	neg_in(F,NF).
neg_in(forall(V,F),forall(V,NF)) :-
	neg_in(F,NF).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\Predicate neg_in_neg/2(Formula, Result).

\PL*/
neg_in_neg(true,false).
neg_in_neg(false,true).
neg_in_neg(not(F),NF) :-
	neg_in(F,NF).
neg_in_neg(predicate(P),not(predicate(P))).
neg_in_neg((A and B),(NA or NB)) :-
	neg_in_neg(not(A),NA),
	neg_in_neg(not(B),NB).
neg_in_neg((A or B),(NA and NB)) :-
	neg_in_neg(not(A),NA),
	neg_in_neg(not(B),NB).
neg_in_neg((A implies B),(NA and NB)) :-
	neg_in_neg(A,NA),
	neg_in_neg(not(B),NB).
neg_in_neg((A iff B),(NA iff NB)) :-
	neg_in_neg(not(A),NA),
	neg_in_neg(B,NB).
neg_in_neg(exists(V,F),forall(V,NF)) :-
	neg_in_neg(not(F),NF).
neg_in_neg(forall(V,F),exists(V,NF)) :-
	neg_in_neg(not(F),NF).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
