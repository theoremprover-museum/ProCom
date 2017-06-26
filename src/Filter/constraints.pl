%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: constraints.pl,v 1.10 1995/03/23 14:04:39 gerd Exp $
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

:- module_interface(constraints). /*%------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.10 $}

This module implements constraint handling for \ProCom{} using the \eclipse{}
mataterm facility.

\PL*/
:- export constraints/2.
:- meta_attribute(constraints, [unify:unify_constraint/2,
				print:print_constraint/2]).
:- global unify_constraint/2,
	  print_constraint/2,
	  get_constraint/2,
	  add_constraints/2.
:- begin_module(constraints).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
info(filter,
	"$Revision: 1.10 $",
	"Filter for contraints handling with ECLiPSe meta terms.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	import setarg/3 from sepia_kernel.
:-	lib(lists),
	lib(linker),
	lib(op_def),
	lib(options),
	lib(eq_member).

:-	op( 500,  fx, '$'  ),
	op(1200, xfx, (//:)). % Constraint separator
	
:- define_option constraint_theory_file = '...constraint_theory.pl'.
:- define_option constraint_enumerate_solutions = on.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate constraints/2(+InStream, +OutStream).

This predicate conforms to the \ProCom{} filter specification. In a
failure-driven loop clauses are read form the stream |InStream|. Depending on
the state either the matrix is translated or the constraint theory stored.



\PL*/
constraints(_,_) :-
	( is_option(prover,Prover), \+ Prover = procom(_) ->
	    err("Constraints: Incompatible prover selected: ",Prover),
	    fail
	; \+ is_option(prolog,Prolog), \+ Prolog = eclipse ->
	    err("Constraints: Incompatible Prolog dialect selected: ",Prolog),
	    fail
	),
	!,fail.
constraints(Stream,OutStream) :-
	setval('CST:state',matrix),
	is_option(constraint_theory_file,File),
	open(File,write,CSTStream),
	set_option('ProCom:put_pretty'=on),
	set_option('ProCom:verbose'=off),
	set_option(apply_reductions = off),
	set_option(connect_weak_unifyable = off),
	is_option('ProCom::post_link',PL),
	get_flag(cwd,Path),
	concat_atom([Path,File],LinkFile),
	( member(LinkFile,PL) ->
	    true
	;   set_option('ProCom::post_link'=[LinkFile|PL])
	),
	is_option('ProCom::immediate_link',IL),
	( member("lib_constraints.pl",IL) ->
	    true
	;   set_option('ProCom::immediate_link'=["lib_constraints.pl"|IL])
	),

	( is_option(constraint_enumerate_solutions) ->
	    is_option('ProCom:post_goal_list',PGL),
	    set_option('ProCom:post_goal_list'=[enumerate_cst|PGL])
	;   true
	),
	writeclause(CSTStream,(:-module('constraint theory'))),
	repeat,
	read(Stream,Clause),
	( Clause = end_of_file ->
	    true
	; Clause = (# begin(constraint_theory)) ->
	    setval('CST:state',constraint_theory),
	    fail
	; Clause = (# end(constraint_theory)) ->
	    setval('CST:state',matrix),
	    fail
	; Clause = (# _) ->
	    writeclause(OutStream,Clause),
	    fail
	; getval('CST:state',matrix) ->
	    ( Clause = (C //: Cst) ->
		term_variables(C,Vars),
		translate_prolog(Cst,Cst2),
		conjunction_to_list(Cst2,CstList),
		cluster(Vars,CstList)
	    ;	C = Clause
	    ),
	    printf(OutStream,"%vQDMw.\n",[C]),
	    fail
	; Clause = (Head <- Tail) ->
	    translate_prolog((Head:-Tail),NewH),
	    keep(NewH),
	    fail
	;
	    translate_prolog(Clause,NewH),
	    keep(NewH),
	    writeclause(CSTStream,NewH),
	    fail
	),
	write_kept(CSTStream),
	close(CSTStream),

	( current_module('constraint theory') ->
	    true
	;   create_module('constraint theory')
	),
	call(compile(File),'constraint theory'),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate cluster/2(+VarList, +ConstraintList).

For each variable in |VarList| collect the relevant constraints from the list
|ConstraintList| and add an appropriate constraint to the variable.

\PL*/
cluster([],_).
cluster([V|R],Cst) :-
	cluster(Cst,[V],_,[],_,Vcst),
	list_to_conjunction(Vcst,Constraint),
%	printf("%w --> %w%n",[V,Constraint]),
	add_attribute(V,constraints:constraints(Constraint)),
	cluster(R,Cst).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate cluster/6(+ConstraintList,
		     +VarList, -NewValList,
		     +Delayed, -StillDelayed,
		     -Constraints).

\PL*/
cluster([],Vars,Vars,Delayed,Delayed,[]) :- !.
cluster([A|B],Vars,VarsOut,Delayed,DelayedOut,Cst) :-
	member(V,Vars),
	occurs(V,A),
	!,
	term_variables((Vars+A),SomeMoreVars),
	eq_subtract(SomeMoreVars,Vars,NewVars),
	( NewVars == [] ->
	    StillDelayed = Delayed,
	    EvenMoreVars = NewVars,
	    VarsB	 = SomeMoreVars,
	    Cst		 = [A|CstB] 
	;   cluster(Delayed,NewVars,EvenMoreVars,[],StillDelayed,MoreCst),
	    term_variables((SomeMoreVars+EvenMoreVars),VarsB),
	    append([A|MoreCst],CstB,Cst)
	),
	cluster(B,VarsB,VarsOut,StillDelayed,DelayedOut,CstB).
cluster([A|B],Vars,VarsOut,Delayed,DelayedOut,Cst) :-
	cluster(B,Vars,VarsOut,[A|Delayed],DelayedOut,Cst).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate conjunction_to_list/2(+Conjunction, ?List).

Translate a conjunction to a list. |Conjunction| can either be a compound with
the functor |,/2| or it is considered as a literal. The conjunction can be
nested left or right. All literals are collected and unified with |List|.

\PL*/
conjunction_to_list(C,Lst) :-
	( C = (A,B) ->
	    conjunction_to_list(A,La),
	    append(La,Lb,Lst),
	    conjunction_to_list(B,Lb)
%	; C = true ->
%	    Lst = []
	;   Lst = [C]
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate list_to_conjunction/2(+List, ?Conjunction).

\PL*/
list_to_conjunction([],true).
list_to_conjunction([P],P) :- !.
list_to_conjunction([P|R],(P,RC)) :-
	list_to_conjunction(R,RC).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate translate_prolog/2(+Prolog, ?NewProlog).

This predicate translates a subset of Prolog by prepending |cst_| to any
literal which does not appear to be a built-in predicate. For this purpose the 
term |Prolog| is decomposed and the arguments which are recognized as formulas
are further translated.

\PL*/
translate_prolog((A:-B),(LA:-LB)) :-
	!,
	translate_prolog(A,LA),
	translate_prolog(B,LB).
translate_prolog((A,B),(LA,LB)) :-
	!,
	translate_prolog(A,LA),
	translate_prolog(B,LB).
translate_prolog((A;B),(LA;LB)) :-
	!,
	translate_prolog(A,LA),
	translate_prolog(B,LB).
translate_prolog((A->B),(LA->LB)) :-
	!,
	translate_prolog(A,LA),
	translate_prolog(B,LB).
translate_prolog((\+ A),(\+ LA)) :-
	!,
	translate_prolog(A,LA).
translate_prolog(not(A),not(LA)) :-
	!,
	translate_prolog(A,LA).
translate_prolog(setof(V,A,W),setof(V,LA,W)) :-
	!,
	translate_prolog(A,LA).
translate_prolog(findall(V,A,W),findall(V,LA,W)) :-
	!,
	translate_prolog(A,LA).
translate_prolog(bagof(V,A,W),bagof(V,LA,W)) :-
	!,
	translate_prolog(A,LA).
translate_prolog($ Literal,Literal) :-
	!.
translate_prolog({Literal},Literal) :-
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Given a literal |Literal| prepend the string |cst_| before the functor. Thus
the effect of a module is achieved without explicitly using one.

\PL*/
translate_prolog(Literal,NewLiteral) :-
	Literal =..  [Functor|Arity],
	concat_atoms('cst ',Functor,NewFunctor),
	NewLiteral =.. [NewFunctor|Arity].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\PL*/
:- dynamic 'KEPT'/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate keep/1(+Clause).

\PL*/
keep((A:-B)) :-
	\+ functor(B,',',2),
	!,
	assert('KEPT'(A,B)).
keep(_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate write_kept/1(+Stream).

\PL*/
write_kept(Stream) :-
	( \+ 'KEPT'(_,_) ->
	    writeclause(Stream,reduce([],[]))
	;
	    retract('KEPT'(A,B)),
	    writeclause(Stream,(reduce(A,B):- -?->true)),
	    fail
	;   true
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\subsection{Constraint Handling}


\Predicate print_constraint/2().

\PL*/
print_constraint(constraint(List),Attr) :-
	-?->
	Attr = List.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\Predicate unify_constraint/2(+Term, ?Attr).

The unify handler is called with the term |Term| that was unified with a
metaterm and its attribute |Attr|. The metaterm variable has already been
unified with |Term|.

The attribute |Attr| is not the whole attribute vector but the slot handled by
this predicate. This attribute can either be a variable or a compound. A
variable indicates that this slot is not used. Thus nothing has to be done.

If |Attr| is a compound we inspect the term |Term| and act accordingly. This
task is performed by the predicate |unify_constraint_term/2|.

\PL*/
unify_constraint(_,Attr) :-
	var(Attr).
unify_constraint(Term,Attr) :-
	compound(Attr),
	unify_constraint_term(Term,Attr).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate unify_constraint_term/2(Term, Attr).

This predicate is called by |unify_constraint/2|. Now we are in the situation
that |Attr| is known to be a compound. The compounds handled by this predicate
have the functor |constraints/1| where the argument is a conjunction of
constraint goals.

If |Term| is not a variable then we simply evaluate the constraint |CST| to
see wether it is satisfyable. This may leave a choice point and provide
additional solutions upon backtracking!

The other case we have to consider occurs when |Term| is another metaterm. In
this case we access its attribute using a matching clause --- indicated by the
|-?->| guard. The attributes are given to |unify_constraint_constraint| for
further inspection.

\PL*/
unify_constraint_term(Term,constraints(CST)) :-
	nonvar(Term),
%	writeln((Term = CST)),		  
	call(CST,'constraint theory').
unify_constraint_term(_{AttrY},AttrX) :-
	-?->
	unify_constraint_constraint(AttrX, AttrY).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate unify_constraint_constraint/2(AttrX, AttrY).

This predicate is called from |unify_constraint_term/2| with the attributes of
the two metaterms which are unified. Now we have to see wether the second
metaterm has already an attribute. If there is no attribute |AttrY| is a
variable. In this case we simply bind it to the attribute |AttrX| of the first
variable.

The remaining case deals with the combination of two constraints. If they are
already identical nothing has to be done. Otherwise the attributes are
combined and simplified. Afterwards it is checked wether the combined
constraint is satisfiable by evaluating it and abolishing the bindings
afterwards. Finally |setarg/3| is used to destructively store the result in
the attribute of the combined metaterms.

\PL*/
unify_constraint_constraint(AttrX, AttrY) :-
	var(AttrY),
	AttrX = AttrY.
unify_constraint_constraint(constraints(CST_X), AttrY) :-
	nonvar(AttrY),
	AttrY = constraints(CST_Y),
	( CST_X == CST_Y ->
	    true
	;
	    simplify_constraint(CST_X,CST_Y,CST),
	    call((\+ \+ CST),'constraint theory'),
	    setarg(1,AttrY,CST)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate simplify_constraint/3(Constraint1, Constraint2, Combined).

The constraints |(Constraint1| and |Constraint2| are combined and simplified.
It is assumed that both constraints are already simplified. Additionally it is
assumed that they are satisfiable. This is assured by the construction of new
constraints with exception of those constraints which are extracted from the
matrix.



According to the construction of the constraints it holds that if one of the
parts to be combined has become ground then
\begin{itemize}
  \item This constraint has already been checked.
  \item It does not contribute any further to the solution.
\end{itemize}
Thus it can be ignored.


\PL*/
simplify_constraint((A,B),Cst2,Simple) :-
	!,
	simplify_one_constraint(A,Cst2,SimpleA),
	simplify_constraint(B,SimpleA,Simple).
simplify_constraint(Cst1,Cst2,Simple) :-
	( \+ nonground(Cst1) ->
	    Simple = Cst2
	; \+ nonground(Cst2) ->
	    Simple = Cst1
	;
	    simplify_one_constraint(Cst1,Cst2,Simple)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate simplify_one_constraint/3().

\PL*/
simplify_one_constraint((A,B),Cst2,Simple) :-
	!,
	( Cst2 == A ->
	    Simple = B
	;
	    Simple = (A,SB),
	    simplify_one_constraint(Cst2,B,SB)
	).
simplify_one_constraint(Cst1,Cst2,Simple) :-
	( Cst1 == Cst2 ->
	    Simple = Cst1
	; \+ nonground(Cst2) ->
	    Simple = Cst1
	; call(reduce(Cst1,Cst2),'constraint theory') ->
	    Simple = Cst2
	; call(reduce(Cst2,Cst1),'constraint theory') ->
	    Simple = Cst1
	;   Simple = (Cst1,Cst2)	  
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_constraint/2().

\PL*/
get_constraint(_{constraints:Attribute}, A) :-
	-?->
	A = Attribute.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_constraints/2().

\PL*/
add_constraints(Var,Attr) :-
%	 writeln(add_constraints),
	add_attribute(Var,Attr).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate enum_cst/1(+List).


\PL*/
enum_cst([]).
enum_cst([_=H|T]) :-
%	printf("::: %MQDw\n",[H]),
	( meta(H) ->
	    get_constraint(H,constraints(CST)),
	    call(CST,'constraint theory')
	;   true
	),
	enum_cst(T).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
