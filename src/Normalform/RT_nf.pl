%%*****************************************************************************
%% $Id: RT_nf.pl,v 1.1 1995/04/06 12:53:14 gerd Exp $
%%*****************************************************************************
%% Author: Gerd Neugebauer
%%=============================================================================

:- true. /*%--- Just to make pcode happy. -------------------------------------

\PL*/
:- module(nf).
:- compile(nf).
:- use_module('../Test/regression_test').
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
regression_max_solutions(64).
regression_equivalent_solutions(X,X).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
regression_test(" Testing predicate varlist/1.",
	1,
	1).
regression_test(varlist([]),
	x,
	[]).
regression_test(varlist([v]),
	x,
	[]).
regression_test(varlist([X,v]),
	x,
	[]).
regression_test(varlist([X]),
	X,
	1).
regression_test(varlist([X,Y]),
	X-Y,
	1).
regression_test(" Testing predicate normal/2.",
	1,
	1).
regression_test(normal((false => X),Result),
	Result,
	[true]).
regression_test(normal((true => something),Result),
	Result,
	[something]).
regression_test(normal((something => true),Result),
	Result,
	[true]).
regression_test(normal((something => false),Result),
	Result,
	[~something]).
regression_test(normal((forall X : p(X)),Result),
	Result,
	[p(X)]).
regression_test(normal((exists X : (forall X : p(X))),Result),
	Result,
	[p(X)]).
regression_test(normal((forall X : (exists X : p(X))),Result),
	Result,
	[p('$kolem1')]).
regression_test(normal((forall X : p(X), exists X : p(X)),Result),
	Result,
	[(p(X), p('$kolem2'))]).
regression_test(normal((forall Y : p(X), exists X : p(X,Y)),Result),
	Result,
	[(p(X), p('$kolem3'(Y), Y))]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- regression_test.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */
