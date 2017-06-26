%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: pool_test.pl,v 1.1 1994/11/28 21:15:11 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module_interface(options).
:- export is_option/2.
:- begin_module(options).

is_option(search,iterative_deepening(1,1,1)).

:- module_interface(message).
:- export err/1.
:- begin_module(message).
err(X) :- writeln(X).

:- module_interface(literal).
:- export negate_literal/2.
:-	op(1045, fx,  (++)),
	op(1045, fx,  (--)).
:- begin_module(literal).

negate_literal(++Literal,--Literal).
negate_literal(--Literal,++Literal).

:- module_interface(matrix).
:- export 'GoalClause'/1, 'Clause'/3.
:- begin_module(matrix).
:- use_module(literal).
'GoalClause'(1).
'Clause'(--p(_g835), [literal(-- p(f(_g835)), 1 - 2)], 1 - 1).
'Clause'(--p(f(_g839)), [literal(-- p(_g839), 1 - 1)], 1 - 2).
'Clause'(++p(_g835), [literal(++ p(f(f(_g835))), 2 - 2)], 2 - 1).
'Clause'(++p(f(f(_g843))), [literal(++ p(_g843), 2 - 1)], 2 - 2).

'HashClause'(I,L,B) :- 'Clause'(L,B,I).
%	    printf("%Dw",[Proof])

:- module(log).
:- module(time).
:- module(hook).

:- module(pool).
:- use_module(matrix),
   use_module(literal),
   use_module(options),
   use_module(message).
:- compile(pool).
:- module(pool).
:- global pool/0.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */
