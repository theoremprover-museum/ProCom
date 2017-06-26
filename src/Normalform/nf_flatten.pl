%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: nf_flatten.pl,v 1.2 1995/04/10 19:24:50 gerd Exp $
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

:- module_interface(nf_flatten). /*%-------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.2 $}

\PL*/
:- export flatten_formula/2.
:- begin_module(nf_flatten).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First we load some libraries. Primarily this is done to get some operators
declared appropriately.

\PL*/
:-	lib(nf_intern).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\Predicate flatten_formula/2(+Formula, ?FlatFormula).

\PL*/
flatten_formula(true,true).
flatten_formula(false,false).
flatten_formula(predicate(F),predicate(F)).
flatten_formula(not(F),not(NF)) :-
	flatten_formula(F,NF).
flatten_formula((A and B),Flat) :-
	( A = and(C,D) ->
	    flatten_formula((C and (D and B)),Flat)
	;   Flat = (FA and FB),
	    flatten_formula(A,FA),
	    flatten_formula(B,FB)
	).
flatten_formula((A or B),Flat) :-
	( A = (C or D) ->
	    flatten_formula((C or (D or B)),Flat)
	;   Flat = (FA or FB),
	    flatten_formula(A,FA),
	    flatten_formula(B,FB)
	).
flatten_formula((A implies B),(FA implies FB)) :-
	flatten_formula(A,FA),
	flatten_formula(B,FB).
flatten_formula((A iff B),(FA iff FB)) :-
	flatten_formula(A,FA),
	flatten_formula(B,FB).
flatten_formula(exists(V,F),exists(V,FF)) :-
	flatten_formula(F,FF).
flatten_formula(forall(V,F),forall(V,FF)) :-
	flatten_formula(F,FF).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
