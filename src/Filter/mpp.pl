%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: mpp.pl,v 1.3 1995/03/06 23:04:17 gerd Exp $
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

:- module_interface(mpp). /*%--------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}

The filter {\tt mpp} provides a {\em M}\/acro {\em P}\/re{\em P}\/rocessor
similar to the C preprocessor |cpp|. It is possible to define macros which are
expanded in the remainder of the file. Inclusion of other files and
conditional parts can also be specified. Last but not least it is possible to
define additional operators for syntactic sugar.

The filter {\tt mpp} provides additional meta instructions. Those meta
instructions are evaluated by this filter and not passed through. The meta
instructions start with the symbol |#|.\footnote{This is defined as a prefix
  operator in \ProTop.} The following meta instructions are evaluated by {\tt
  mpp}: 

\begin{description}
\item [\#include {\em File}.]\ \\
  The contents of file {\em File} is included instead of this instruction as
  if it was there already. Nevertheless only complete clauses can be contained
  in this file. It is not possible to include parts of clauses.

  The included files are searched with the same algorithm as files specified
  as input files. I.e. if they are not absolute then they are searched in the
  directories given in the option
  |input_path|\index{input\_path}. Additionally the extensions stored in the
  option |input_extensions|\index{input\_extensions} are taken into account.

  The following example includes the file {\sf common}:

\begin{BoxedSample}
  \#include "common".
\end{BoxedSample}

\item [\#op({\em Precedence}, {\em Associativity}, {\em Name}\/).]\ \\
  For the remainder of this file the operator {\em Name} with precedence {\em
    Precedence} and associativity {\em Associativity} is defined. See the
  Prolog documentation on |op/3| for details.

  {\bf Note:} This operator is not automatically defined in the next filter!

  It might be a good practice to define a translation rule for any
  operator. Thus it can be translated into a ``normal'' operator not written
  in infix, postfix, or prefix notation. This helps avoiding confusion in the
  following parts of the system.

  The following example defines an operator |==>| which is used
  afterwards. Without the declaration this example would lead to an syntax
  error.

\begin{BoxedSample}
  \#op(900,xfx,(==>)).
  p(X) ==> q(X,f(X)).
  q(11,32).
\end{BoxedSample}

\item [\#define {\em Head} = {\em Tail}.]\ \\
  The macro definition specified in this instruction is stored. This does not
  overwrite previous definitions but is added. In the macro expansion the
  first unifying definition is applied.  The usual Prolog rules for variables
  are used.

  The definitions are applied at every level. This means that they are used at
  the formula level as well as on the term level.

\begin{BoxedSample}
  \#define a = new\_a.
  \#define p(X,new\_a) = pa(X).

  q(A).
  p(X,a).
  a.
\end{BoxedSample}
  The result is
\begin{BoxedSample}
  q(\_g123).
  p(\_g123,new\_a).
  new\_a.
\end{BoxedSample}

  To complete our example started in section on operator declaration we can
  define a translation rule (macro) to translate |==>| terms into the normal
  implication form supported by \ProTop.

\begin{BoxedSample}
  \#op(900,xfx,(==>)).
  \#define (A ==> B) = (B :- A).
  p(X) ==> q(X,f(X)).
\end{BoxedSample}

\item [\#define {\em Head}.]\ \\
  This is the same as defining {\em Head}\/ to 1. This is especially useful in
  combination with conditional inclusion of clauses (see below).

\begin{BoxedSample}
  \#define use\_equality\_axioms
\end{BoxedSample}

\item [\#undef {\em Pattern}.]\ \\
  Since macros are added but not overwritten we need a way to get rid of a
  macro. The undef instruction removes all macros for which the head is
  unifiable with {\em Pattern}. If no matching macro is found nothing is done.

  In the following example two macros are defined and the first one is deleted
  afterwards. 
\begin{BoxedSample}
  \#define mac(1,X) m1(X).
  \#define mac(2,X) m2(X).
  \#undef mac(1,\_).
\end{BoxedSample}
  The following instruction would have deleted both macros:
\begin{BoxedSample}
  \#undef mac(\_,\_).
\end{BoxedSample}

\item [\#if {\em Condition}.]\ \\
  {\em Condition}\/ is evaluated. If it evaluates to true then the block to
  the next matching |#else| or |#endif| is used. The |#else| to |#endif| block
  is ignored. Otherwise the block between the next matching |#else| and the
  |#endif| is used.

\begin{BoxedSample}
  \#if defined(use\_equality\_axioms).
  \#include equality\_axioms.
  \#endif.
\end{BoxedSample}

  They following conditionals are recognized by the |#if| instruction:

  \begin{description}
  \item [defined({\em Pattern}\/)]\ \\
    This condition evaluates to true iff a macro is defined for which the head
    is unifiable with {\em Pattern}.

  \item [is\_option({\em Option}\/)]\ \\
    This condition evaluates to true iff the option {\em Option}\/ is not
    turned off. See the implementation description of the predicate
    |is_option/1| for details.

  \item [not({\em Condition})]\ \\
    This condition evaluates to true iff {\em Condition} does not evaluate to
    true.
  \item [{\em Condition},{\em Condition}]\ \\
    A conjunction evaluates to true iff all conjuncts evaluate to true.

  \item [{\em Condition};{\em Condition}]\ \\
    A disjunction evaluates not to true iff at least one disjunct evaluates
    not to true.
  \end{description}

\item [\#else.]\ \\
  Switch to the alternate block started with a previously matching |#if|,
  |#ifdef|, or |#ifndef|.

\item [\#endif.]\ \\
  This instruction ends a block started with |#if|, |#ifdef|, or |#ifndef|.

\item [\#ifdef {\em Spec}.]\ \\
  This is the same as |#if defined(|{\em Spec}\/|)|.
  
\item [\#ifndef {\em Spec}.]\ \\
  This is the same as |#if not(defined(|{\em Spec}\/|))|.
\end{description}

The filter {\tt mpp} provides the hook |mpp_hook/1| which is called just
before the first clause is read. This hook can be used to add definitions or
include files at startup under the control of a Prolog program. For this
purpose several Prolog procedures are exported.

\PL*/
:- export mpp/2,
	  mpp_op/3,
	  mpp_define/2,
	  mpp_undefine/1,
	  mpp_undef/1,
	  mpp_include/2.
:- begin_module(mpp).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Our identification as a filter for \ProTop.

\PL*/
info(filter,"$Revision: 1.3 $","Syntactic sugar. C like preprocessor.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We need some modules from the system.

\PL*/
:-	lib(hook),
	lib(op_def),
	lib(options),
	lib(find_file).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We define a hook to perform tasks just before the first clause is read. This
can be used from within Prolog. The argument passed to the hook is the ouput
stream in use.

\PL*/
:- define_hook mpp_hook/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We add out own syntactic sugar and define some operators.

\PL*/
:-	op(0,xfx,(if)),		% Hide global definition
	op(0,xfx,(else)),	% Hide global definition
	op(0,xfx,(endif)),	% Hide global definition
	op(750,fx,(if)),
	op(750,fx,(ifdef)),
	op(750,fx,(ifndef)),
	op(750,fx,(define)),
	op(750,fx,(undef)),
	op(750,fx,(undefine)),
	op(750,fx,(include)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate MPP Macro/2(Head,Tail).

\Predicate MPP Stack/3(Condition,Value,OldValue).

\PL*/
:-	dynamic 'MPP Macro'/2,
		'MPP Stack'/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The \eclipse{} global variable |mpp_mode| is used to store the current
mode. The supported values are |pass| and |ignore|.

\PL*/
:-	make_local_array(mpp_mode).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate mpp/2(+Stream, +OutStream).

Terms are transfered from the input stream |Stream| to the output stream
|OutStream| until the end of file is found.

\PL*/
mpp(Stream,OutStream) :-
	setval(mpp_mode,pass),
	retract_all('MPP Macro'(_,_)),
	retract_all('MPP Stack'(_,_,_)),
	run_hooks(mpp_hook,OutStream),
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	; Term = (#MPP) ->
	    once(do_mpp(MPP,OutStream)),
	    fail
	;   getval(mpp_mode,pass),
	    rewrite_mpp(Term,OutTerm),
	    writeclause(OutStream,OutTerm),
	    fail
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate mpp_op/3(+Precedence, +Associativity, +Name).

\PL*/
mpp_op(A,B,C) :-
	op(A,B,C).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate mpp_define/2(+Head,+Body).

\PL*/
mpp_define(Head,Body) :-
	assert('MPP Macro'(Head,Body)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate mpp_define/1(Head).

\PL*/
mpp_define(Head) :-
	assert('MPP Macro'(Head,1)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate mpp_undefine/1(Pattern).

\PL*/
mpp_undefine(Pattern) :-
	retract_all('MPP Macro'(Pattern,_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate mpp_undef/1(Pattern).

\PL*/
mpp_undef(Pattern) :-
	retract_all('MPP Macro'(Pattern,_)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate mpp_include/2(+File, +Stream).

\PL*/
mpp_include(File,OutStream) :-
	is_option(input_path,Path),
	is_option(input_extensions,Extensions),
	find_file(File,Path,Extensions,Full),
	open(Full,read,Stream),
	mpp(Stream,OutStream),
	close(Stream),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate do_mpp/2(+Instruction, +Stream).

\PL*/
do_mpp(Var,_) :-
	var(Var),
	!.
do_mpp(define(Head = Body),_) :-
	!,
	assert('MPP Macro'(Head,Body)).
do_mpp(define(Head),_) :-
	!,
	assert('MPP Macro'(Head,1)).
do_mpp(undefine(Head),_) :-
	!,
	retract_all('MPP Macro'(Head,_)).
do_mpp(undef(Head),_) :-
	!,
	retract_all('MPP Macro'(Head,_)).
do_mpp(include(File),OutStream) :-
	!,
	mpp_include(File,OutStream).
do_mpp(op(A,B,C),_) :-
	!,
	op(A,B,C).
do_mpp(ifdef(F/A),_) :-
	!,
	do_mpp(if(defined(F/A))).
do_mpp(ifndef(F/A),_) :-
	!,
	do_mpp(if(not(defined(F/A)))).
do_mpp(if(Cond),_) :-
	!,
	getval(mpp_mode,Old),
	( evaluate_condition(Cond) ->
	    setval(mpp_mode,pass),
	    asserta('MPP Stack'(Cond,pass,Old))
	;   setval(mpp_mode,ignore),
	    asserta('MPP Stack'(Cond,ignore,Old))
	).
do_mpp(else,_) :-
	!,
	once('MPP Stack'(_,Value,_)),
	( Value = pass ->
	    setval(mpp_mode,ignore)
	;   setval(mpp_mode,pass)
	).
do_mpp(endif,_) :-
	!,
	retract('MPP Stack'(_,_,Old)),
	setval(mpp_mode,Old).
do_mpp(Etc,OutStream) :-
	!,
	writeclause(OutStream,(# Etc)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate evaluate_condition/1(+Condition).

\PL*/
evaluate_condition(defined(Head)) :-
	once('MPP Macro'(Head,_)).
evaluate_condition(is_option(Option)) :-
	is_option(Option).
evaluate_condition(not(Cond)) :-
	\+ evaluate_condition(Cond).
evaluate_condition((A,B)) :-
	evaluate_condition(A),
	evaluate_condition(B).
evaluate_condition((A;B)) :-
	(   evaluate_condition(A)
        ;   evaluate_condition(B)
        ).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate rewrite_mpp/2(Term,NewTerm).

\PL*/
rewrite_mpp(Term,New) :-
	( var(Term) ->
	    New = Term
	; once('MPP Macro'(Term,Expanded)) ->
	    rewrite_mpp(Expanded,New)
	;   functor(Term,F,A),
	    functor(New,F,A),
	    rewrite_mpp(A,Term,New)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate rewrite_mpp/3().

\PL*/
rewrite_mpp(0,_,_) :- !.
rewrite_mpp(1,Term,New) :- 
	!,
	arg(1,Term,A1),
	arg(1,New,B1),
	rewrite_mpp(A1,B1).
rewrite_mpp(2,Term,New) :- 
	!,
	arg(1,Term,A1),
	arg(1,New,B1),
	rewrite_mpp(A1,B1),
	arg(2,Term,A2),
	arg(2,New,B2),
	rewrite_mpp(A2,B2).
rewrite_mpp(N,Term,New) :- 
	!,
	arg(N,Term,A),
	arg(N,New,B),
	rewrite_mpp(A,B),
	N1 is  N - 1,
	rewrite_mpp(N1,Term,New).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
