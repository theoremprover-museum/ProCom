%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: nf_normal.pl,v 1.3 1995/04/24 21:29:11 gerd Exp $
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

:- module_interface(nf_normal). /*%--------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}

\PL*/
:- export normal/3.
:- begin_module(nf_normal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First we load some libraries. Primarily this is done to get some operators
declared appropriately.

\PL*/
:- 	lib(eq_member),
	lib(nf_intern).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate normal/3(+Formula, -Normal, -Variables).

\PL*/
normal(true,true,[]).
normal(false,false,[]).
normal(predicate(P), predicate(P),V) :-
	term_variables(P,V).
normal(not(F), Normal, V) :-
	normal_neg(F, Normal, V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
normal((F and G), Normal, V) :-
	normal(F, NF, VF),
	normal(G, NG, VG),
	conjunction(NF,NG,Normal),
	( atom(Normal) ->
	    V = []
	;   eq_union(VG,VF,V)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
normal((F or G), Normal,V) :-
	normal(F, NF, VF),
	normal(G, NG, VG),
	disjunction(NF,NG,Normal),
	( atom(Normal) ->
	    V = []
	;   eq_union(VG,VF,V)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following equivalence is used to treat implications:
\begin{eqnarray*}
  F\IMPLIES G &\iff& \neg F\OR G
\end{eqnarray*}
\PL*/
normal((F implies G), Normal,V) :-
	normal((not(F) or G), Normal,V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following equivalences are used to simplify equivalences:
\begin{eqnarray*}
  \top\IFF F &\iff& F\\
  \bot\IFF F &\iff& \neg F\\
  F\IFF \top &\iff& F\\
  F\IFF \bot &\iff& \neg F\\
  F\IFF G    &\iff& (F\AND G)\OR(\neg F\AND \neg G)
\end{eqnarray*}
\PL*/
normal((F iff G), Normal,V) :-
	normal(F,NF,VF),
	( NF == true ->
	    normal(NG,Normal,V)
        ; NF == false ->
	    normal(not(NG),Normal,V)
	;   normal(G,NG,VG),
	    ( NG == true ->
		Normal = NF,
		V =VF
	    ; NF == false ->
		normal(not(NF),Normal,V)
	    ; 
		Normal = or(NF,NG),
		eq_union(VG,VF,V),

		rename_bound_variables(NF,F2),
		rename_bound_variables(NG,G2),
		normal(((NF and NG) or (not(F2) and not(G2))), Normal,V)
	    )
        ).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following equivalences are used to simplify equivalences:
\begin{eqnarray*}
  \Forall x\Forall y F &\iff& \Forall y\Forall x F\\
  \Forall x F &\iff& F
\end{eqnarray*}
The second equivalence holds if $x$\/ does not occur in $F$.\footnote{This
  assumes that the universe is not empty.}

As a consequence we have also the following equivalences without any
additional effort:
\begin{eqnarray*}
  \Forall x \top &\iff& \top\\
  \Forall x \bot &\iff& \bot
\end{eqnarray*}
\PL*/
normal(forall(VL,F), Normal, V) :-
	normal(F,NF,VF),
	eq_subtract(VF,VL,V),
	eq_intersect(VL,VF,NVL),
	( NVL == [] ->
	    Normal = NF
	;   Normal = forall(NVL,NF)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following equivalences are used to simplify equivalences:
\begin{eqnarray*}
  \Exists x\Exists y F &\iff& \Exists y\Exists x F\\
  \Exists x F &\iff& F
\end{eqnarray*}
The second equivalence holds if $x$\/ does not occur in $F$.\footnote{This
  assumes that the universe is not empty.}

As a consequence we have also the following equivalences without any
additional effort:
\begin{eqnarray*}
  \Exists x \top &\iff& \top\\
  \Exists x \bot &\iff& \bot
\end{eqnarray*}
\PL*/
normal(exists(VL,F), Normal, V) :-
	normal(F,NF,VF),
	eq_subtract(VF,VL,V),
	eq_intersect(VL,VF,NVL),
	( NVL == [] ->
	    Normal = NF
	;   Normal = exists(NVL,NF)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate normal_neg/3(+Formula, ?Normalized, ?Variables).

This predicate performs the normalization of the formula |Formula| with an
implicit negation. The result is unified with |Normalized|. The free variables
of |Normalized| are unified with |Variables|.

We use the following equivalence.
\begin{eqnarray*}
  \neg \top &\iff& \bot
\end{eqnarray*}
\PL*/
normal_neg(true,false,[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{eqnarray*}
  \neg \bot &\iff& \top
\end{eqnarray*}
\PL*/
normal_neg(false,true,[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For a predicate the negation has to be made explicit. All variables occurring
in the predicate are free.

\PL*/
normal_neg(predicate(P), not(predicate(P)), V) :-
	term_variables(P,V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{eqnarray*}
  \neg \neg F &\iff& F
\end{eqnarray*}
\PL*/
normal_neg(not(F), Normal, V) :-
	normal(F, Normal, V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{eqnarray*}
  \neg (F\AND G) &\iff& \neg F \OR \neg G
\end{eqnarray*}
\PL*/
normal_neg((F and G), Normal, V) :-
	normal((not(F) or not(G)), Normal, V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{eqnarray*}
  \neg (F\OR G) &\iff& \neg F \AND \neg G
\end{eqnarray*}
\PL*/
normal_neg((F or G), Normal, V) :-
	normal(and(not(F),not(G)), Normal, V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{eqnarray*}
  \neg (F\IMPLIES G) &\iff& F \AND \neg G
\end{eqnarray*}
\PL*/
normal_neg((F implies G), Normal, V) :-
	normal(and(F,not(G)), Normal, V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{eqnarray*}
  \neg (F\IFF G) &\iff& (F \AND \neg G)\OR(\neg F \AND G)
\end{eqnarray*}
\PL*/
normal_neg((F iff G), Normal, V) :-
	rename_bound_variables(F,F2),
	rename_bound_variables(G,G2),
	normal(((F and not(G)) or (not(F2) and G2)), Normal, V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{eqnarray*}
  \neg (\Exists x F) &\iff& \Forall x \neg F
\end{eqnarray*}
\PL*/
normal_neg(exists(X,F), Normal, V) :-
	normal(forall(X,not(F)), Normal, V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{eqnarray*}
  \neg (\Forall x F) &\iff& \Exists x \neg F
\end{eqnarray*}
\PL*/
normal_neg(forall(X,F), Normal, V) :-
	normal(exists(X,not(F)), Normal, V).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
