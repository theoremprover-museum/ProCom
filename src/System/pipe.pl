%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: pipe.pl,v 1.7 1995/01/11 09:15:45 gerd Exp $
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

:- module_interface(pipe). /*%-------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.7 $}

\subsection{Pipe Clauses in Prolog}

Prolog is built build upon the assumption that clauses denote
alternatives for the execution of a goal. Now we want to consider
rewriting of a term. In this case we want that all ``clauses'' are
applied in turn to the input to produce the output. Note that each
``clauses'' is tried once and that the clauses are applied in the
sequence of definition.

To distinguish pipe clauses from usual clauses a special syntax is
used.\footnote{This is similar to Definite Clause Grammars (DCGs) in
standart Prolog.} Those special clauses are translated into ordenary
Prolog clauses which are processed as usual.

The syntax we have choosen consists of two types of rules. The first
type is of the form

\begin{itemize}\item [] {\em Type|:| Input |==>| Output |.|}
\end{itemize}

{\em Type}, {\em Input}, and {\em Output} are arbitrary Prolog terms.
{\em Type} is used for two purposes. First it can be used to
distinguish several sets of rewriting rules and second, it can be used
to carry arguments.

The procedural view of such a rule is that it is simply unified
against a query. If this unification succeeds then the result of the
unification is the result of the application. Otherwise {\em Input}
and {\em Output} are unified.

The first kind of rewrite rules closely correspond to facts in Prolog.
Thus we want to allow something like rules. Rules are characterized by
an additional body of Prolog goals. We are using the operator |where|
to separate the body from the initial part.

\begin{itemize}\item [] {\em Type|:| Input |==>| Output |where| Body |.|}
\end{itemize}

Procedurally {\em Type}, {\em Input}, and {\em Output} are unified
with a calling goal. If this unification succeeds then {\em Body} is
executed as a Prolog goal. If this goal succeeds then the result is
found. Otherwise {\em Input} and {\em Output} are unified.

Before we can start we have to declare the types of rewrite rules we
want to use. This is done with the predicate |rewrite_define| which
takes as argument a pair {\em Functor|/|Arity} for the {\em Type}
term. For convenience |rewrite_define| is declared to be a prefix
operator.

Let us consider some examples.

\begin{SampleCode}{80mm}
\begin{verbatim}
  :- [pipe.pl].
  :- define_pipe sample/0.

  sample: 47 ==> 56.
  sample: N  ==> Np1 where
	  number(N),
	  Np1 is N+1.
  sample: this ==> that
\end{verbatim}
\end{SampleCode}


*** To be completed

\subsubsection*{Implementation}

Now we need some declarations for the syntax described above.
\PL*/
:- op(1190,xfx,(==>)).
:- op(1150,xfx,(where)).
:- op(1150,fx,'define_pipe').

:- dynamic 'Pipe Index'/3.
:- dynamic (==>)/2.

:- global 'Pipe Expand'/2.
:- global (define_pipe)/2.

:- tool((define_pipe)/1,(define_pipe)/2).

%:- define_macro((:-)/2,'Pipe Expand'/2,[local,clause]).
:- define_macro((==>)/2,'Pipe Expand'/2,[local,clause]).

:- begin_module(pipe).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate define_pipe/1(+Type/+Arity).

Define a pipe type. The driver predicate is asserted and the clause counter
initialized to 1. Any proviously defined predicates for this type are
abolished first.
\PL*/
define_pipe(Type/Arity,Module) :-
%	write(define_pipe(Type/Arity,Module)),nl,
	assert('Pipes'(Type,Arity,Module)),
	functor(Template,Type,Arity),
	'Pipe make driver'(Template,N,Max,In,Out,Driver),
	'Pipe make head'(Template,N1,In,Out1,Goal),
	'Pipe make driver'(Template,N1,Max,Out1,Out,Driver_true),
	'Pipe make driver'(Template,N1,Max,In,Out,Driver_false),
	functor(Driver,DF,DA),
	functor(Goal  ,GF,GA),
	( call('Pipe Index'(Type,Arity,_),Module) ->
	    call(retract_all('Pipe Index'(Type,Arity,_)),Module),
	    call(abolish(DF/DA),Module),
	    call(abolish(GF/GA),Module)
	;
	    call(dynamic(DF/DA),Module),
	    call(dynamic(GF/GA),Module)
	),
	call(assert('Pipe Index'(Type,Arity,1)),Module),
	call(assert((Driver :- 
		N1 is N+1,
		( N > Max ->  Out = In
		; Goal	  ->  Driver_true
		;	      Driver_false
		))),Module),
	N=0,
	call(assert((Template:In==>Out :-
			'Pipe Index'(Type,Arity,Max),
			Driver,
			!
		   )),Module).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Pipe make head/5(+Template, +A1, +A2, +A3, -Term).

This predicate constructs a new term by adding a prefix to the functor
and three additional arguments to {\em Template}. The resulting term
is {\em Term}.
\PL*/
'Pipe make head'(Template,A1,A2,A3,Term) :-
	Template =.. [Functor|Args],
	concat_atoms('Pipe : ',Functor,NewFunctor),
	Term	 =.. [NewFunctor,A1,A2,A3|Args].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Pipe make driver/5(+Template, +A1, +A2, +A3, +A4, -Term).

This predicate constructs a new term by adding a prefix to the functor
and four additional arguments to {\em Template}. The resulting term is
{\em Term}.
\PL*/
'Pipe make driver'(Template,A1,A2,A3,A4,Term) :-
	Template =.. [Functor|Args],
	concat_atoms('Pipe :: ',Functor,NewFunctor),
	Term	 =.. [NewFunctor,A1,A2,A3,A4|Args].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Pipe expand rule head/4(+Type, +In, +Out, -Pred).

\PL*/
'Pipe expand rule head'(Type,In,Out,Pred) :-
	functor(Type,Functor,Arity),
	'Pipes'(Functor,Arity,Module),
	(   call(retract('Pipe Index'(Functor,Arity,N)),Module) -> 
	    N1 is N+1
	;   printf(error,"*** Pipe type %w undeclared%n",
		   [Functor/Arity]),
	    N  = 1,
	    N1 = 2
	),
	call(assert('Pipe Index'(Functor,Arity,N1)),Module),
	'Pipe make head'(Type,N,In,Out,Pred).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Pipe Expand/2(+From, -To).

Finally we define expansion rules for the two types of rewriting. They
are anchored in the Prolog reading apparatus with the primitive
|add_expansion/1| from the Prolog library |add_portray|.

\PL*/
'Pipe Expand'((Type: In ==> Out where Body), (Goal:-Body)) :-
	'Pipe expand rule head'(Type, In, Out, Goal).

'Pipe Expand'((Type: In ==> Out :- Body), (Goal:-Body)) :-
	'Pipe expand rule head'(Type, In, Out, Goal).

'Pipe Expand'((Type: In ==> Out), (Goal)) :-
	'Pipe expand rule head'(Type, In, Out, Goal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

%:- define_macro((==>)/2,'Pipe Expand'/2,[local,clause]).

\EndProlog */
