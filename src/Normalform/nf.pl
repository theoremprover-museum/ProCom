%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: nf.pl,v 1.3 1995/04/24 21:29:11 gerd Exp $
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

:- module_interface(nf). /*%---------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}

\def\iff{\Leftrightarrow}
\def\deduce{\vdash}

{\catcode`\|12
\gdef\IR{\begin{center}\begin{tabular}{|p{7em}|p{15em}|p{12em}|}\hline
%  {\footnotesize Formula}&
%  {\footnotesize External Representations}&
%  {\footnotesize Internal Representation}
%  \\\hline\hline%
  }
\gdef\endIR{\\\hline\end{tabular}\end{center}}
}


We use negative representation. Thus the proof problem

\[
	\varphi_1\ANDdots\varphi_n\deduce\psi
\]
is transformed into
\[
	\varphi_1\AND\ldots\AND\varphi_n\AND¬\psi\deduce\bot
\]

We have to produce a conjunctive normal form. The clauses --- which are sets
of literals --- represent disjunctions of literals.


Examples of translations:

\begin{tabular}{ll}
  {\tt ?- Goal}		& \([Goal]\)			\\
  {\tt L}		& \([¬L]\)			\\
  {\tt Head :- B1,B2,B3}& \([¬Head, B_1, B_2, B_3]\)	\\
  {\tt L1 -> L2}	& \([L_1, ¬L2]\)		\\
  {\tt L1, L2}		& \([L_1, L2]\)			\\
  {\tt L1; L2}		& \([L_1], [L2]\)		\\
\end{tabular}
\bigskip

\PL*/
:- export nf/2,
	  nf/3.
:- begin_module(nf).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This module identifies itself as a filter for \ProTop.
\PL*/
info(filter,
     "$Revision: 1.3 $",
     "Filter to perfom a normal-form transformation.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First we load some libraries. Primarily this is done to get some operators
declared appropriately.

\PL*/
:- 	lib(options),
	lib(message),
	lib(eq_member).
:-	lib(nf_intern),
	lib(nf_normal),
	lib(nf_neg_in),
	lib(nf_closure),
	lib(nf_def_pg),
	lib(nf_skolem),
	lib(nf_anti_prenexing),
	lib(nf_flatten).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The option |nf:heuristic| is defined. It can either be a name of a built-in
heuristic or a list of primitive instructions. The value of this option is
used as program if now is given as argument to the predicate |nf|.

\PL*/
:- define_option 'nf:heuristic' = de_morgan.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate nf_heuristic/2(+Name, ?Program).

This predicate is used to define certain heuristics for the normal form
transformation. Thus a user can specify just the symbolic name and does not
need to bother with the details.

\PL*/
nf_heuristic(de_morgan,
	     [ rename_variables,
	       closure,
	       normal,
	       anti_prenexing,
	       skolemization
	     ]).
nf_heuristic(simple,
	     [ rename_variables,
	       closure,
	       normal,
	       skolemization
	     ]).
nf_heuristic(pg,
	     [ rename_variables,
	       closure,
	       def_pg,
	       normal,
	       skolemization
	     ]).
nf_heuristic(trace(Heuristic), [nl,tee("intern\t%w")|Instructions]) :-
	( functor(Heuristic,'.',2) ->
	    Instr = Heuristic
	;   nf_heuristic(Heuristic,Instr)
	),
	atom(Heuristic),
	findall([I,tee(I,"\t%w")],member(I,Instr),Instructions).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate nf/2(+Stream, +OutStream).

This is the filter which transforms the input formula into normal form. The
value of the option |nf:heuristic| is used to specify the heuristic to be
used.

\PL*/
nf(Stream,OutStream) :-
	is_option('nf:heuristic',Heuristic),
	nf(Heuristic,Stream,OutStream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate nf/3(+Heuristic, +Stream, +OutStream).

This is the filter which transforms the input formula into normal form. The
heuristic to be used is specified as the argument |Heuristic|. This argument
can either be a symbolic name for a heuristic or a list of instructions.

\PL*/
nf(Heuristic,Stream,OutStream) :-
	( nf_heuristic(Heuristic,NF_Prog) ->
	    true
	; functor(Heuristic,'.',2) ->
	    NF_Prog = Heuristic
	; Heuristic == [] ->
	    NF_Prog = Heuristic
	; nf_heuristic(Default,NF_Prog) ->
	    msg("*** Filter nf: There is no heuristic named ", Heuristic,
	        "\n--- Using the default heuristic: ", Default)
	;   err("*** Filter nf: There is no heuristic named ",Heuristic),
	    fail
	),
	repeat,
	read(Stream,Formula),
	( Formula = end_of_file ->
	    true
	; Formula = (# begin(Section)) ->
	    write_clause(Formula,OutStream),
	    skip_section(Stream,OutStream,Section),
	    fail
	; Formula = (# end(Section)) ->
	    write_clause(Formula,OutStream),
	    err("*** Filter nf Error: isolated #",end(Section)),
	    fail
	; functor(Formula,(#),1) ->
	    write_clause(Formula,OutStream),
	    fail
	; functor(Formula,(.),2) ->
	    put_clause(Formula,OutStream),
	    fail
	; ( Formula = (?- F),
	    functor(F,'.',2) ) ->
	    put_clause(Formula,OutStream),
	    fail
	;
	    intern_representation(~Formula,Form),
	    nf_interpreter(NF_Prog,Form,NormalForm),
	    get_clause(NormalForm,Clause,[]),
	    mult_taut(Clause,SimpleClause),
	    printf(OutStream,"[",[]),
	    put_literals(SimpleClause,OutStream),
	    printf(OutStream,"].\n",[]),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
mult_taut([--H|T],Res) :-
	( eq_member(++H,T) ->
	    fail
	; eq_member(--H,T) ->
	    mult_taut(T,Res)
	;   Res = [--H|R],
	    mult_taut(T,R)
	).
mult_taut([++H|T],Res) :-
	( eq_member(--H,T) ->
	    fail
	; eq_member(++H,T) ->
	    mult_taut(T,Res)
	;   Res = [++H|R],
	    mult_taut(T,R)
	).
mult_taut([],[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate skip_section/3(+Stream, +OutStream, +Section).

This predicate copies the Prolog terms read form the stream |Stream| to the
output stream |OutStream| until the term |#end(|{\em Section}\/|)| is
encountered. 

\PL*/
skip_section(Stream,OutStream,Section) :-
	repeat,
	read(Stream,Formula),
	( Formula = end_of_file ->
	    true
	; Formula = (# end(Section)) ->
	    write_clause(Formula,OutStream)
	; Formula = (# begin(NewSection)) ->
	    write_clause(Formula,OutStream),
	    skip_section(Stream,OutStream,NewSection),
	    fail
	;   write_clause(Formula,OutStream),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_clause/3(+Formula, -List, ?Tail).

This predicate extracts a single clause form a formula |Formula| in negation
normal form. This formula uses the internal representation. If the formula
contains quantifiers or strange junctors. Those branches fail and may result
in incomplete sets of clauses.

The special predicates |true| and |false| are evaluated properly.
They do not appear in the clause.

The output is a difference list in the form of two arguments denoting the list
|List| itself and its tail |Tail|. Thus it is possible to avoid additional
calls to |append/3|.

\PL*/
get_clause(true,X,X).
get_clause(predicate(P),[++P|X],X).
get_clause(not(F),[--P|X],X):-
	( F = predicate(P) ->
	    true
	;   err("*** Filter nf: Error in get_clause. Strange branch ignored:",
		not(F)),
	    fail
	).
get_clause((A and B),D,L) :-
	get_clause(A,D,X),
	get_clause(B,X,L).
get_clause((A or B),D,L) :-
	(   get_clause(A,D,L)
	;   get_clause(B,D,L)
	).
get_clause(impl(V,F),_,_) :-
	err("*** Filter nf: Error in get_clause. Strange branch ignored:\n*** ",
	    impl(V,F)),
	fail.
get_clause(iff(V,F),_,_) :-
	err("*** Filter nf: Error in get_clause. Strange branch ignored:\n*** ",
	    iff(V,F)),
	fail.
get_clause(exists(V,F),_,_) :-
	err("*** Filter nf: Error in get_clause. Strange branch ignored:\n*** ",
	    exists(V,F)),
	fail.
get_clause(forall(V,F),_,_) :-
	err("*** Filter nf: Error in get_clause. Strange branch ignored:\n*** ",
	    forall(V,F)),
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate put_literals/2(+LiteralList, +Stream).

This predicate prints a list of literals |LiteralList| to the output stream
|Stream|. The predicates are indented one TAB and separated with commas. The
enclosing list brackets are not printed.

\PL*/
put_literals([],_).
put_literals([Pred|T],Stream) :-
	( T == [] ->
	    printf(Stream,"\t%QDVw\n",[Pred])
	;   printf(Stream,"\t%QDVw,\n",[Pred]),
	    put_literals(T,Stream)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate nf_interpreter/3(+Instruction, +InForm, ?OutForm).

The normal form interpreter takes a single instruction or a list of
instruction and executes them. In the case of a single instruction the input
|InForm| is transformed and the result is unified with the output
|OutForm|. In the case of a list the input is given to the first member. The
output of the first member goes to the second member and so on. Thus the
interpreter acts like a pipe.

\PL*/
nf_interpreter([],F,F) :-
	!.
nf_interpreter([H|T],Fin,Fout) :-
	!,
	nf_interpreter(H,Fin,F),
	nf_interpreter(T,F,Fout).
nf_interpreter(loop(Instructions),Fin,Fout) :-
	!,
	nf_interpreter(Instructions,Fin,F),
	( Fin == F ->
	    Fout = F
	;   nf_interpreter(loop(Instructions),F,Fout)
	).
nf_interpreter(nl,F,F) :-
	!,
	nl.
nf_interpreter(writeln(Format),F,F) :-
	!,
	writeln(Format).
nf_interpreter(tee(Prefix,Format),F,F) :-
	!,
	write(Prefix),
	printf(Format,F),
	nl.
nf_interpreter(tee(Format),F,F) :-
	!,
	printf(Format,F),
	nl.
nf_interpreter(normal,Fin,Fout) :-
	!,
	normal(Fin,Fout,_).
nf_interpreter(closure,Fin,Fout) :-
	!,
	closure(Fin,Fout).
nf_interpreter(anti_prenexing,Fin,Fout) :-
	!,
	anti_prenexing(Fin,Fout).
nf_interpreter(skolemization,Fin,Fout) :-
	!,
	skolemization(Fin,Fout).
nf_interpreter(neg_in,Fin,Fout) :-
	!,
	neg_in(Fin,Fout).
nf_interpreter(flatten,Fin,Fout) :-
	!,
	flatten_formula(Fin,Fout).
nf_interpreter(rename_variables,Fin,Fout) :-
	!,
	rename_bound_variables(Fin,Fout).
nf_interpreter(def_pg,Fin,Fout) :-
	!,
	def_pg(Fin,Fout).
nf_interpreter(Program,Fin,Fout) :-
	nf_heuristic(Program,Instructions),
	!,
	nf_interpreter(Instructions,Fin,Fout).
nf_interpreter(Instruction,F,F) :-
	err("*** Filter nf Warning. Instruction not found: ",Instruction).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
