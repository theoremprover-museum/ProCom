%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: literal.pl,v 1.12 1995/02/13 20:05:14 gerd Exp $
%%%============================================================================
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

:- module_interface(literal). /*%----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.12 $}

This module provides predicates to manipulate literals.

A literal is a predicate with the additional sign |++| or |--|.

\PL*/
:- export negate_literal/2,
	  make_literal/4,
	  merge_literal_signs/2,
	  'Macro literal_is_positive'/2,
	  'Macro literal_is_negative'/2,
	  'Macro literal_functor'/2,
	  'Macro literal_sign'/2,
	  'Macro literal_neg_sign'/2.

:- define_macro(literal_is_positive/1,	'Macro literal_is_positive'/2,	[]).
:- define_macro(literal_is_negative/1,	'Macro literal_is_positive'/2,	[]).
:- define_macro(literal_functor/3,	'Macro literal_functor'/2,	[]).
:- define_macro(literal_sign/2,		'Macro literal_sign'/2,		[]).
:- define_macro(literal_neg_sign/2,	'Macro literal_neg_sign'/2,	[]).

:-	op(1045, fx,  (++)),
	op(1045, fx,  (--)).

:- begin_module(literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate negate_literal/2(Literal, NegLiteral).

This  predicate	 succeeds if the  literals  |Literal| and |NegLiteral|	can be
unified except of the reversed signs.

\PL*/
negate_literal(++Literal,--Literal).
negate_literal(--Literal,++Literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_literal/4(Functor, Arity, Sign, Literal).

This predicate	  unifies  |Literal| with    the literal  having  the  functor
|Functor|, the arity |Arity|  and the sign |Sign|.  The	 arguments have	 to be
sufficiently instanciated or a runtime error will be raised.

\PL*/
make_literal(Functor,Arity,'++',++Literal) :-
	functor(Literal,Functor,Arity).
make_literal(Functor,Arity,'--',--Literal) :-
	functor(Literal,Functor,Arity).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate literal_is_positive/1(+Literal).

This procedure	tries	to determine  wether  the  given  literal is positive.
|Literal| should be sufficiently instanciated.

For efficiency reasons this procedure is implemented as an \eclipse{} macro.

\PL*/
'Macro literal_is_positive'(no_macro_expansion(literal_is_positive(Literal)),
			    functor(Literal,'++',1)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate literal_is_negative/1(+Literal).

This procedure	tries	to determine  wether  the  given  literal is negative.
|Literal| should be sufficiently instanciated.

For efficiency reasons this procedure is implemented as an \eclipse{} macro.

\PL*/
'Macro literal_is_negative'(no_macro_expansion(literal_is_negative(Literal)),
			    functor(Literal,'--',1)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate literal_functor/2(+Literal, ?Functor).

This procedure extracts the functor |Functor| from a literal |Literal|.

For efficiency reasons this procedure is implemented as an \eclipse{} macro.

\PL*/
'Macro literal_functor'(no_macro_expansion(literal_functor(Literal,F,A)),
			(arg(1,Literal,X),functor(X,F,A))).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate literal_sign/2(+Literal, ?Sign).

This procedure extracts the sign |Sign|	 from the literal |Literal|. |Literal|
has to be a proper literal. The sign is one of the symbols |++| or |--|.

For efficiency reasons this procedure is implemented as an \eclipse{} macro.

\PL*/
'Macro literal_sign'(no_macro_expansion(literal_sign(Literal,Sign)),
		     functor(Literal,Sign,1)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate literal_neg_sign/2(+Literal, ?NegSign).

This procedure extracts the  sign from the  literal |Literal| and returns  the
negated sign in |NegSign|. |Literal| has to  be a proper  literal. The sign is
one of the symbols |++| or |--|.

For efficiency reasons this procedure is implemented as an \eclipse{} macro.

\PL*/
'Macro literal_neg_sign'(no_macro_expansion(literal_neg_sign(Literal,Sign)),
		     (functor(Literal,'++',1) -> Sign = '--'; Sign = '++')).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate merge_literal_signs/2(Literal, Normalized).

This predicate	tries  to  normalize a	literal |Literal|.  The	 signs may  be
missing or multiple signs are present. Those signs are collapsed into a single
sign. Additionally the sign |-| is allowed instead of |--|.

\PL*/
merge_literal_signs(V,V) :- 
	var(V),
	!.
merge_literal_signs(-L,NewL)  :-
	!,
	merge_literal_signs_neg(L,NewL).
merge_literal_signs(--L,NewL)  :-
	!,
	merge_literal_signs_neg(L,NewL).
merge_literal_signs(++L,NewL)  :-
	!,
	merge_literal_signs_pos(L,NewL).
merge_literal_signs(L,++L).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate merge_literal_signs_neg/2(Literal,Normal).

\PL*/
merge_literal_signs_neg(L,--L) :-
	var(L),
	!.
merge_literal_signs_neg(-L,NewL) :-
	!,
	merge_literal_signs(L,NewL).
merge_literal_signs_neg(--L,NewL) :-
	!,
	merge_literal_signs(L,NewL).
merge_literal_signs_neg(++L,NewL) :-
	!,
	merge_literal_signs_neg(L,NewL).
merge_literal_signs_neg(L,--L).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate merge_literal_signs_pos/2(Literal,Normal).

\PL*/
merge_literal_signs_pos(L,(++L)) :-
	var(L),
	!.
merge_literal_signs_pos(-L,NewL) :-
	!,
	merge_literal_signs((--L),NewL).
merge_literal_signs_pos(--L,NewL) :-
	!,
	merge_literal_signs((--L),NewL).
merge_literal_signs_pos(++L,NewL) :-
	!,
	merge_literal_signs((++L),NewL).
merge_literal_signs_pos(L,(++L)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped 'Macro literal_functor'/2,
	   'Macro literal_is_negative'/2,
	   'Macro literal_is_positive'/2,
	   'Macro literal_neg_sign'/2,
	   'Macro literal_sign'/2,
	   make_literal/4,
	   merge_literal_signs/2,
	   negate_literal/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
