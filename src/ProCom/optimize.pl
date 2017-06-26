%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: optimize.pl,v 1.19 1995/02/13 20:05:14 gerd Exp $
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

:- module_interface(optimize). /*
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.19 $}

\PL*/
:- export put_optimized_clause/1,
	  clear_definitions/0,
	  store_definition/3. 
:- begin_module(optimize).

:-	lib(options),
	lib(p_put),
	lib(maplist).

:-	use_module(linker).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_optimized_clause/1(+ProcList).

\PL*/
put_optimized_clause(ProcList) :-
	( is_option('ProCom:optimize') ->
	    (	get_clause(ProcList,Clause),
		put_simplified_clause(Clause),
		fail
	    ;	true
	    )
	;
	    maplist(put_simplified_clause,ProcList)
	),
	put_nl.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_simplified_clause/1(+Clause).

Write a clause to the output stream. Simplifications are applied to
remove superfluous |true| subgoals.

\PL*/
put_simplified_clause(Comment,Clause) :-
	puts(Comment),
	put_simplified_clause(Clause).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_simplified_clause/1(+Clause).

\PL*/
put_simplified_clause((Head :- Body)) :-
	!,
	simplify(Body,SimpleBody),
	( SimpleBody = true ->
	    put_clause(Head)
	;   put_clause((Head :- SimpleBody))
	).
put_simplified_clause((:- Body)) :-
	!,
	simplify(Body,SimpleBody),
	put_clause((:- SimpleBody)).
put_simplified_clause((?- Body)) :-
	!,
	simplify(Body,SimpleBody),
	put_clause((?- SimpleBody)).
put_simplified_clause(Head) :-
	put_clause(Head).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate simplify/2(+Term,?Simplified).

This predicate takes a term and tries to simplify it. This is done primarily
be decomposing the term and using knowledge on the leading functor.

A conjunction is simplified by simplifying the conjuncts. |true| predicates
are ignored. Branches following a |fail| are ignored aswell.

\PL*/
simplify((A,B),Simple) :-
	!,
	simplify(A,SA),
	simplify(B,SB),
	( SA = true ->
	    Simple = SB
	; SA = fail ->
	    Simple = fail
	; SB = true ->
	    Simple = SA
	;   Simple = (SA,SB)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If-then-else constructs can be simplified considerably if the condition
evaluates to |true| or |fail|. In this case the then or the else branch can be
omitted.

\PL*/
simplify((A->B;C),Simple) :-
	!,
	simplify(A,SA),
	( SA = true ->
	    simplify(B,Simple)
	; SA = fail ->
	    simplify(C,Simple)
	;   simplify(B,SB),
	    simplify(C,SC),
	    Simple = (SA->SB;SC)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Disjunctions can be simplified if one of the brances fails. In this
case only the other branch is left.
\PL*/
simplify((A;B),Simple) :-
	!,
	simplify(A,SA),
	simplify(B,SB),
	( SA = fail ->
	    Simple = SB
	; SB = fail ->
	    Simple = SA
	;   Simple = (SA;SB)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
simplify((\+A),Simple) :-
	!,
	simplify(A,SA),
	( SA = true ->
	    Simple = fail
	; SA = fail ->
	    Simple = true
	;   Simple = (\+ SA)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Next we declare how some simple literals can be evaluated. This list
of built-in predicates and conditions when to evaluate them is rather
ad hoc and mainly motivated by my current needs. A more complete list
of possible simplifications is desirable.

\begin{description}
  \item [functor/3]\ \\
	This predicate is evaluated when either the first argument or the
	second and third arguments are instanciated.
\end{description}
\PL*/
simplify(functor(T,F,A),Simple) :-
	nonvar(T),
	!,
	(functor(T,F,A) ->
	    Simple = true
	;   Simple = fail
	).
simplify(functor(T,F,A),Simple) :-
	nonvar(F),
	nonvar(A),
	!,
	(functor(T,F,A) ->
	    Simple = true
	;   Simple = fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [arg/3]\ \\
	This predicate is evaluated when position and term are sufficiently
	instanciated.
\end{description}
\PL*/
simplify(arg(N,T,Arg),Simple) :-
	nonvar(T),
	integer(N),
	!,
	functor(T,_,A),
	( (N>0, N=<A) ->
	    arg(N,T,X),
	    Simple = (X=Arg)
	;   Simple = fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [name/1]\ \\
	This predicate is evaluated when the first argument is instanciated.
\end{description}
\PL*/
simplify(name(A,B),Simple) :-
	nonvar(A),
	!,
	( name(A,B) ->
	    Simple = true
	;   Simple = fail
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [=/2]\ \\
	This predicate performs unification. If both sides are sufficiently 
	instanciated then the unification problem is decomposed. If two 
	variables are given then one is enough. The unification is performed
	and no code returned.
\end{description}
\PL*/
simplify((A=B),Simple) :-
	nonvar(A),
	nonvar(B),
	!,
	functor(A,F,Arity),
	( functor(B,F,Arity) ->
	    decompose_unify(Arity,A,B,S),
	    simplify(S,Simple)
	;   Simple = fail
	).
simplify((A=B),true) :-
	var(A),
	var(B),
	!,
	A=B.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [$\setminus$==/2]\ \\
	This predicate is evaluated if the arguments are sufficiently
	instanciated.
\end{description}
\PL*/
simplify((A\==B),Simple) :-
	atomic(A),
	atomic(B),
	A \== B,
	!,
	Simple = true.
simplify((A==B),Simple) :-
	atomic(A),
	atomic(B),
	!,
	( A == B ->
	    Simple = true
	;   Simple = fail
	).
simplify((A==B),Simple) :-
	nonvar(A),
	nonvar(B),
	!,
	functor(A,Fa,Aa),
	functor(B,Fb,Ab),
	( ( Fa \== Fb
	  ; Aa \== Ab )->
	    Simple = fail
	;   Simple = (A==B)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [nonvar/1]\ \\
	If the argument is already instanciated at optimization time then
	this predicate evaluates to |true|.
\end{description}
\PL*/
simplify(nonvar(A),Simple) :-
	nonvar(A),
	!,
	Simple = true.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [nonground/1]\ \\
	If the argument is already nonground at optimization time then
	this predicate evaluates to |true|.
\end{description}
\PL*/
simplify(nonground(A),Simple) :-
	\+ nonground(A),
	!,
	Simple = fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [ground/1]\ \\
	If the argument is already ground at optimization time then
	this predicate evaluates to |true|.
\end{description}
\PL*/
simplify(ground(A),Simple) :-
	\+ nonground(A),
	!,
	Simple = true.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\begin{description}
  \item [var/1]\ \\
	If the argument is already instanciated at optimization time then
	this predicate evaluates to |fail|.
\end{description}
\PL*/
simplify(var(A),Simple) :-
	nonvar(A),
	!,
	Simple = fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
simplify((A=..B),true) :-
	nonvar(A),
	!,
	A=..B.
simplify((A=..B),true) :-
	nonvar(B),
	B = [F|_],
	nonvar(F),
	!,
	A=..B.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
simplify(X is A,Simple) :-
	!,
	Simple = (X is SA),
	simplify_arithm(A,SA).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
simplify(L =< R, Simple) :-
	!,
	simplify_arithm(L,Ls),
	simplify_arithm(R,Rs),
	( (integer(Ls), integer(Rs)) ->
	    (Ls =< Rs -> Simple = true
	    ;		 Simple = fail
	    )
	; Ls == Rs ->
	    Simple = true
	;   Simple = (Ls =< Rs)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
simplify(X < A, X < SA) :-
	!,
	simplify_arithm(A,SA).

simplify(X >= A,Simple) :-
	!,
	Simple = (X >= SA),
	simplify_arithm(A,SA).

simplify(X > A,Simple) :-
	!,
	Simple = (X > SA),
	simplify_arithm(A,SA).

simplify(integer(X), Simple) :-
	nonvar(X),
	!,
	( integer(X) ->
	    Simple = true
	;   Simple = fail
	).


simplify(X =:= A,Simple) :-
	!,
	Simple = (X =:= SA),
	simplify_arithm(A,SA).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
simplify(once(true),true) :-
	!.
simplify(once(fail),fail) :-
	!.
simplify(once(G),once(Simple)) :-
	!,
	simplify(G,Simple).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If a definition for a term is known then we replace the term by its
simplified definition.
\PL*/
simplify(A,Simple) :-
	'Definition'(A,S,_),
	!,
	simplify(S,Simple).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finally we describe the case that no simplifications are applicable. In this
case the term is returned unchanged.

\PL*/
simplify(A,A).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate simplify_arithm/2(Term, SimplifiedTerm).

\PL*/
simplify_arithm(A,A) :- 
	var(A),
	!.
simplify_arithm(A+B,C) :- 
	!,
	simplify_arithm(A,SA),
	simplify_arithm(B,SB),
	( SA == 0 -> C = SB
	; SB == 0 -> C = SA
	; integer(SA),integer(SB) -> C is SA + SB
	; C = SA + SB).
simplify_arithm(A-B,C) :- 
	!,
	simplify_arithm(A,SA),
	simplify_arithm(B,SB),
	( SA == 0 -> C = -SB
	; SB == 0 -> C = SA
	; integer(SA),integer(SB) -> C is SA - SB
	; C = SA - SB).
simplify_arithm(A*B,C) :- 
	!,
	simplify_arithm(A,SA),
	simplify_arithm(B,SB),
	( SA == 0 -> C = 0
	; SB == 0 -> C = 0
	; SA == 1 -> C = SB
	; SB == 1 -> C = SA
	; integer(SA),integer(SB) -> C is SA * SB
	; C = SA * SB).
simplify_arithm(X,X).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate decompose_unify/4(+Pos, +Term1, +Term2, -Result).

\PL*/
decompose_unify(0,_,_,true).
decompose_unify(1,Term1,Term2,(A1=A2)) :-
	arg(Term1,1,A1),
	arg(Term2,1,A2).
decompose_unify(N,Term1,Term2,Result) :-
	N > 1,
	Result = ((A1=A2),Rest),
	arg(Term1,N,A1),
	arg(Term2,N,A2),
	N1 is N-1,
	decompose_unify(N1,Term1,Term2,Rest).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- dynamic 'Definition'/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate store_definition/3(+Head, +Body, +File).

\PL*/
store_definition(Head,Body,File) :-
	( is_option('ProCom:expand') ->
	    assert('Definition'(Head,Body,File))
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate clear_definitions/0().

\PL*/
clear_definitions :-
	retract_all('Definition'(_,_,_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_clause/2().

\PL*/
get_clause(Proc,Clause) :-
	get_clause_from_proc(Proc,C,Rest),
	(   Clause = C
	;   get_clause(Rest,Clause)
	).

get_clause_from_proc([Clause],Clause,[]).
get_clause_from_proc([(Head1:-Tail1),(Head2:-Tail2)|Rest],Clause,Others) :-
	( \+ is_meta(Head1), \+ is_meta(Head2), variant(Head1,Head2) ->
	    Head1 = Head2,
	    merge_conjunction(Tail1,Tail2,Tail),
	    get_clause_from_proc([(Head1:-Tail)|Rest],Clause,Others)
	;   Clause = (Head1:-Tail1),
	    Others = [(Head2:-Tail2)|Rest]
	).

is_meta(Term) :-
	term_variables(Term,Vars),
	once((member(X,Vars),meta(X))).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate merge_conjunction/3().

\PL*/
merge_conjunction(true,true,true) :- !.
merge_conjunction(Tail,T,NewTail) :-
	split_conjunction(Tail,A1,A2),
	split_conjunction(T,B1,B2),
	( variant(A1,B1) ->
	    A1 = B1,
	    NewTail = (A1,NT),
	    merge_conjunction(A2,B2,NT)
	;
	    merge_disjunction(Tail,T,NewTail)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate split_conjunction/3().

\PL*/
split_conjunction(Conjunction,C1,C2) :- 
	( Conjunction=(C1,C2) -> true
	;   C1 = Conjunction, 
	    C2 = true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate merge_disjunction/3().

\PL*/
merge_disjunction(Dis1,Dis2,Disjunction) :-
	(Dis1=(D1;D2) ->
	    Disjunction = (D1;Dis),
	    merge_disjunction(D2,Dis2,Dis)
	;   Disjunction = (Dis1;Dis2)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
