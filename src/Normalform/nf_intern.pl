%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: nf_intern.pl,v 1.3 1995/04/24 21:29:11 gerd Exp $
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

:- module_interface(nf_intern). /*%--------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}



\PL*/
:- export intern_representation/2,
	  list_2_conj/2,
	  conj_2_list/2,
	  list_2_disj/2,
	  disj_2_list/2,
	  rename_bound_variables/2,
	  disjunction/3,
	  conjunction/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For syntactic sugar we need a lot of new operators. Those are defined
below. Some operators from {\sf literal.pl} and {\sf op\_defs.pl} are
duplicated here.


\PL*/
:-	op(1200, xfx, (::)),  		% Label separator
	op(1200, xfy, (<=>)),		% Equivalence
	op(1200, xfy, (<->)),		% Equivalence
	op(1200, xfy, (iff)),		% Equivalence
	op(1200, xfy, (equivalent)),	% Equivalence
	op(1200, xfy, (equiv)),		% Equivalence
	op(1190, fx,  (:-)),		% Negation.
	op(1190, fx,  (?-)),		% Negation.
	op(1190,  fx, (<=)),		% Negation.
	op(1190, fx,  (<-)),  		% Negation.
	op(1190, fx,  (if)),		% Negation.
	op(1190, xfy, (implies)),	% Implication.
	op(1190, xfy, (=>)),		% Implication.
	op(1190, xfy, (if)),		% Reverse implikation.
	op(1190, xfx, (:-)),		% Reverse implikation.
	op(1190, xfy, (<=)),		% Reverse implication.
	op(1190, xfx, (<-)),  		% Reverse implication.
	op(1100, xfy, (or)),		% Disjunction.
	op(1000, xfy, (and)),		% Conjunction.
	op(1000, xfy, (&)),		% Conjunction.
	op( 550,  fx, (exists)),	% Existential quantifier.
	op( 550,  fx, (forall)),	% Universal quantifier.
	op( 550,  fx, (all)),		% Universal quantifier.
	op( 520,  fy, (box)),		%  wishful thinking
	op( 520,  fy, (diamond)),	%  wishful thinking
	op(1190, xfx, (-->)),		% Just to get things right.
	op( 800,  fx, (#)).		% Instruction operator.
:-	op(1045,  fx, (++)),
	op(1045,  fx, (--)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(nf_intern).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(eq_member).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate intern_representation/2(+Extern, ?Intern).

This predicate translates an external representation of a formula into the
unique internal representation. During this transformation syntactic sugar is
eliminated. Other normalizations are not performed.

The internal representation uses type functors to mark certain types of
subformulae. This techniques allows us to omit cuts in the following process
of normalization. The leading functor unambigously identifies the type of the
subformula in question.

The table \ref{fig:representations} specifies which types of formulae are
considered and how they are represented.

\begin{table}[htp]
  { \footnotesize
    \begin{IR}
      Formula	& External Representations & Internal Representation
      \\\hline\hline
      $¬A$		&{\tt  not A
        \par --A
        \par -A
        \par \char126 A
        }&{\tt not(A)	}\\\hline
      $A\AND B$	&{\tt A and B
        \par A , B
        \par A \& B
        \par and(A,B)
        }&{\tt A and B}\\\hline
      $A\OR B$	&{\tt A or B
        \par A ; B
        \par A \char124{} B
        \par or(A,B)
        }&{\tt A or B	}\\\hline
      $A\IMPL B$	&{\tt A implies B
        \par A => B
        \par implies(A,B)
        \par B if A
        \par B :- A
        \par B <= A
        \par if(B,A)
        }&{\tt A implies B}\\\hline
      $A\IFF B$	&{\tt A iff B
        \par A <=> B
        \par A <-> B 
        \par iff(A,B)
        \par A equivalent B
        \par equivalent(A,B)
        \par A equiv B
        \par equiv(A,B)
        }&{\tt A iff B	}\\\hline
      $\Exists X F$	&{\tt exists X : F 
        \par exists [X] : F
        \par exists(X,F)
        \par exists([X],F)
        }&{\tt exists([X],F)	}\\\hline
      $\Exists {X_1,\ldots,X_n} F$	&
      {\tt exists [X1,$\ldots$,Xn] : F
        \par exists X1:$\ldots$ exists Xn : F
        \par exists([X1,$\ldots$,Xn],F)
        }&{\tt exists([X1,$\ldots$,Xn],F)	}\\\hline
      $\Forall X F$	&{\tt forall X : F 
        \par forall [X] : F
        \par forall(X,F)
        \par forall([X],F)
        \par all X : F 
        \par all [X] : F
        \par all(X,F)
        \par all([X],F)
        }&{\tt forall([X],F)	}\\\hline
      $\Forall {X_1,\ldots,X_n} F$	&
      {\tt forall [X1,$\ldots$,Xn] : F
        \par forall X1:$\ldots$ forall Xn : F
        \par forall([X1,$\ldots$,Xn],F)
        \par all [X1,$\ldots$,Xn] : F
        \par all X1:$\ldots$ all Xn : F
        \par all([X1,$\ldots$,Xn],F)
        }&{\tt forall([X1,$\ldots$,Xn],F)	}\\\hline
      $\bot$ &{\tt false\par fail	   }&{\tt false	}\\\hline
      $\top$ &{\tt true		   }&{\tt true	}\\\hline
      $P$ $^*$&{\tt P		   }&{\tt predicate(P)	}
\end{IR}}

{\footnotesize $^*$ $P$\/ is a predicate if it is none of the other logical
  constructs.}
  \caption{Representations of formulae}\label{fig:representations}

\end{table}

First of all we check that the formula is not a variable. In this case an
error message is printed and the translation into internal representation
fails. 

\PL*/
intern_representation(X,_) :-
	var(X),
	!,
	err("*** Filter nf: Unexpected variable in formula encountered: ",
	    X,
	    "\n*** Higher order formulae are not supported (yet)."),
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{IR}
  $\top$ &{\tt true		   }&{\tt true	}
\end{IR}

\PL*/
intern_representation(true,true) :-
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{IR}
  $\bot$ &{\tt false
	   \par fail
	  }&{\tt false	}
\end{IR}

\PL*/
intern_representation(false,false) :-
	!.
intern_representation(fail,false) :-
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{IR}
  $¬A$		&{\tt  not A
                  \par --A
		  \par -A
                  \par ~A
                  }&{\tt not(A)	}
\end{IR}

\PL*/
intern_representation(not(A),not(IA)) :-
	!,
	intern_representation(A,IA).
intern_representation(--A,not(IA)) :-
	!,
	intern_representation(A,IA).
intern_representation(~A,not(IA)) :-
	!,
	intern_representation(A,IA).
intern_representation(-A,not(IA)) :-
	!,
	intern_representation(A,IA).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{IR}
  $¬A$	&{\tt :- A
		  \par <- A
		  \par <= A
		  \par ?- A
                  }&{\tt not(A)	}
\end{IR}

\PL*/
intern_representation((:-B),not(IB)) :-
	!,
	intern_representation(B,IB).
intern_representation((<-B),not(IB)) :-
	!,
	intern_representation(B,IB).
intern_representation((<=B),not(IB)) :-
	!,
	intern_representation(B,IB).
intern_representation((?-B),not(IB)) :-
	!,
	intern_representation(B,IB).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{IR}
  $A\AND B$	&{\tt A and B
		  \par A , B
		  \par A \& B
		  \par and(A,B)
                  }&{\tt A and B}
\end{IR}

\PL*/
intern_representation((A,B),(IA and IB)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation((A & B),(IA and IB)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation(and(A,B),(IA and IB)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{IR}
  $A\OR B$	&{\tt A or B
		  \par A ; B
		  \par A \char124{} B
		  \par or(A,B)
                  }&{\tt A or B	}
\end{IR}

\PL*/
intern_representation((A;B),(IA or IB)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation((A or B),(IA or IB)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{IR}
  $A\IMPL B$	&{\tt A implies B
		  \par A => B
		  \par implies(A,B)
		  \par B if A
		  \par B :- A
		  \par B <= A
		  \par if(B,A)
                  }&{\tt A implies B}
\end{IR}

\PL*/
intern_representation((A=>B),(IA implies IB)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation(implies(A,B),(IA implies IB)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation((A->B),(IA implies IB)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation(if(A,B),(IB implies IA)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation((A<=B),(IB implies IA)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation((A:-B),(IB implies IA)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation(if(A,B),(IB implies IA)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{IR}
  $A\IFF B$	&{\tt A iff B
		  \par A <=> B 
		  \par A <-> B 
		  \par iff(A,B)
                  \par A equivalent B
                  \par equivalent(A,B)
                  \par A equiv B
                  \par equiv(A,B)
                  }&{\tt A iff B	}
\end{IR}

\PL*/
intern_representation((A<=>B),(IB iff IA)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation((A<->B),(IB iff IA)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
intern_representation((A iff B),(IB iff IA)) :-
	!,
	intern_representation(A,IA),
	intern_representation(B,IB).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{IR}
  $\Exists X F$	&{\tt exists X : F 
		  \par exists [X] : F
                  \par exists(X,F)
                  \par exists([X],F)
                  }&{\tt exists([X],F)	}\\\hline
  $\Exists {X_1,\ldots,X_n} F$	&
		  {\tt exists [X1,$\ldots$,Xn] : F
                  \par exists X1:$\ldots$ exists Xn : F
                  \par exists([X1,$\ldots$,Xn],F)
		  }&{\tt exists([X1,$\ldots$,Xn],F)	}
\end{IR}

\PL*/
intern_representation(exists(X,A),F) :-
	!,
	intern_representation((exists X:A),F).
intern_representation((exists X:A),exists(VL,IA)) :-
	!,
	intern_vars(X,V1),
	intern_representation(A,A2),
	( A2 = exists(V,IA) ->
	    eq_union(V1,V,VL)
	;   VL = V1,
	    IA = A2
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{IR}
  $\Forall X F$	&{\tt forall X : F 
		  \par forall [X] : F
                  \par forall(X,F)
                  \par forall([X],F)
                  \par all X : F 
		  \par all [X] : F
                  \par all(X,F)
                  \par all([X],F)
                  }&{\tt forall([X],F)	}\\\hline
  $\Forall {X_1,\ldots,X_n} F$	&
		  {\tt forall [X1,$\ldots$,Xn] : F
                  \par forall X1:$\ldots$ forall Xn : F
                  \par forall([X1,$\ldots$,Xn],F)
                  \par all [X1,$\ldots$,Xn] : F
                  \par all X1:$\ldots$ all Xn : F
                  \par all([X1,$\ldots$,Xn],F)
		  }&{\tt forall([X1,$\ldots$,Xn],F)	}
\end{IR}

\PL*/
intern_representation(all(X,A),F) :-
	!,
	intern_representation((forall X:A),F).
intern_representation((all X:A),F) :-
	!,
	intern_representation((forall X:A),F).
intern_representation(forall(X,A),F) :-
	!,
	intern_representation((forall X:A),F).
intern_representation((forall X:A),forall(VL,IA)) :-
	!,
	intern_vars(X,V1),
	intern_representation(A,A2),
	( A2 = forall(V,IA) ->
	    eq_union(V1,V,VL)
	;   VL = V1,
	    IA = A2
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The prefix |++| might be used anywhere. It is simply ignored.

\PL*/
intern_representation(++A,IA) :-
	!,
	intern_representation(A,IA).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The final case is to treat anything else not covered as predicate yet.

\begin{IR}
  $P$&{\tt P		   }&{\tt predicate(P)	}
\end{IR}
\PL*/
intern_representation(A,predicate(A)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate intern_vars/2(+VarSpec, ?VarList).

This predicate translates a variable specification |VarSpec| into the internal
representation of a variable list |VarList|. |VarSpec| can either be a single
variable, a list of variables, or a conjunction of variables.

\begin{BoxedSample}
  ?- intern\_vars(X,List).
  List = [X]
  ?- intern\_vars([X,Y],List).
  List = [X,Y]
  ?- intern\_vars((X,Y),List).
  List = [X,Y]
\end{BoxedSample}

\PL*/
intern_vars(V,[V]) :-
	var(V),
	!.
intern_vars([V|VL],[V|VL]) :-
	var(V),
	!.
intern_vars((V,VL),[V|VL2]) :-
	var(V),
	!,
	intern_vars(VL,VL2).
intern_vars(X,_) :-
	err("*** Filter nf syntax error: Quantification over nonvariable in ",
	    X),
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate conj_2_list/2(Conjunction,List).

This predicate translates a conjunction of formulae |Conjunction| into a list
|List|.

\PL*/
conj_2_list(X,L) :-
	conj_2_list(X,L,[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate conj_2_list/3(Conjunction,List,Tail).

\PL*/
conj_2_list((A and B),L,R) :-
	!,
	conj_2_list(A,L,X),
	conj_2_list(B,X,R).
conj_2_list(F,[F|L],L).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate list_2_conj/2(List,Conjunction).

\PL*/
list_2_conj([],true).
list_2_conj([L|R],Result) :- 
	( R = [] ->
	    Result = L
	;   Result = (L and C),
	    list_2_conj(R,C)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate disj_2_list/2(Disjunction,List).

\PL*/
disj_2_list(X,L) :-
	disj_2_list(X,L,[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate disj_2_list/3(Disjunction,List,Tail).

This predicate translates a disjunction of formulae |Disjunction| into a list
|List|.

\PL*/
disj_2_list((A or B),L,R) :-
	!,
	disj_2_list(A,L,X),
	disj_2_list(B,X,R).
disj_2_list(F,[F|L],L).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate list_2_disj/2(+List, ?Disjunction).

This predicate translates a list of formulae |List| into a disjunction
|Disjunction|.

\PL*/
list_2_disj([],true).
list_2_disj([L],L) :-
	!.
list_2_disj([L|R],(L or C)) :- 
	list_2_disj(R,C).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate rename_bound_variables/2(+Term, ?NewTerm).

\PL*/
rename_bound_variables(true,true).
rename_bound_variables(false,false).
rename_bound_variables(not(P),not(P)).
rename_bound_variables(predicate(P),predicate(P)).
rename_bound_variables((A and B),(NA and NB)) :-
	rename_bound_variables(A,NA),
	rename_bound_variables(B,NB).
rename_bound_variables((A or B),(NA or NB)) :-
	rename_bound_variables(A,NA),
	rename_bound_variables(B,NB).
rename_bound_variables((A implies B),(NA implies NB)) :-
	rename_bound_variables(A,NA),
	rename_bound_variables(B,NB).
rename_bound_variables((A iff B),(NA iff NB)) :-
	rename_bound_variables(A,NA),
	rename_bound_variables(B,NB).
rename_bound_variables(exists(V,F),exists(NewV,NewF)) :-
	rename_vars(F,NewF,V,NewV,_).
rename_bound_variables(forall(V,F),forall(NewV,NewF)) :-
	rename_vars(F,NewF,V,NewV,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate rename_vars/5(+Term, ?NewTerm, ?Vars, ?NewVars, ?RestVars).

\PL*/
rename_vars(Term,NewTerm,Vars,NewVars,RestVars) :-
	term_variables(Term,TermVars),
	copy_term(Term+TermVars,NewTerm+NewTermVars),
	assign_vars(TermVars,NewTermVars,Vars,NewVars,RestVars).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate assign_vars/5(Vars1,Vars2,...).

\PL*/
assign_vars([],[],_,[],[]).
assign_vars([H|T],[H2|T2],Vars,NewVars,RestVars) :-
	( eq_member(H,Vars) ->
	    NewVars = [H2|NV],
	    RestVars = RV
	;   H = H2,
	    NewVars = NV,
	    RestVars = [H2|RV]
	),
	assign_vars(T,T2,Vars,NV,RV).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate conjunction/3().

The following equivalences are used to simplify conjunctions:
\begin{eqnarray*}
  \top\AND F &\iff& F\\
  \bot\AND F &\iff& \bot\\
  F\AND \top &\iff& F\\
  F\AND \bot &\iff& \bot
\end{eqnarray*}

\PL*/
conjunction(true ,    F,F        ) :- !.
conjunction(false,   _F,false    ) :- !.
conjunction(F    , true,F        ) :- !.
conjunction(_F   ,false,false    ) :- !.
conjunction(F    ,    G,(F and G)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate disjunction/3().

The following equivalences are used to simplify disjunctions:

\begin{eqnarray*}
  \top\OR F &\iff& \top\\
  \bot\OR F &\iff& F\\
  F\OR \top &\iff& \top\\
  F\OR \bot &\iff& F
\end{eqnarray*}

\PL*/
disjunction(true ,   _F,true     ) :- !.
disjunction(false,    F,F        ) :- !.
disjunction(_F   , true,true     ) :- !.
disjunction(F    ,false,F        ) :- !.
disjunction(F    ,    G,(F or G)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
