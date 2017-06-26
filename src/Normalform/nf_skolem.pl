%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: nf_skolem.pl,v 1.2 1995/04/10 19:24:50 gerd Exp $
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

:- module_interface(nf_skolem). /*%--------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.2 $}

\PL*/
:- export skolemization/2.
:- begin_module(nf_skolem).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- 	lib(eq_member),
	lib(nf_intern),
	lib(nf_closure).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now we allocate a local Prolog variable. This variable is used to generate new
skolem terms.

\PL*/
:-	make_local_array(gensym),
	setval(gensym,1).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate skolemization/2(+Formula, ?SkolemizedFormula).

\PL*/
skolemization(Formula,SkolemizedFormula) :-
	free_variables(Formula,Vars),
	skolemization(Formula,Vars,SkolemizedFormula).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate skolemization/3(+Formula, +FreeVariables, ?SkolemizedFormula).

\PL*/
skolemization(true,_,true).
skolemization(false,_,false).
skolemization(predicate(F),_,predicate(F)).
skolemization(not(F),_,not(F)).
skolemization((A and B),Vars,(SA and SB)) :-
	skolemization(A,Vars,SA),
	skolemization(B,Vars,SB).
skolemization((A or B),Vars,(SA or SB)) :-
	skolemization(A,Vars,SA),
	skolemization(B,Vars,SB).
skolemization(exists(V,F),Vars,SF) :-
	eq_union(Vars,V,NewVars),
	skolemization(F,NewVars,SF).
skolemization(forall(V,F),Vars,SF) :-
	skolemize_vars(V,Vars),
	skolemization(F,Vars,SF).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate skolemize_vars/2(Vars, FreeVars).

\PL*/
skolemize_vars([],_).
skolemize_vars([Head|Tail],Args) :-
	getval(gensym,No),
	incval(gensym),
	concat_atom(['$kolem',No],Functor),
	Head =.. [Functor|Args],
	skolemize_vars(Tail,Args).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
