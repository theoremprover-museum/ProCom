%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: unify.pl,v 1.7 1995/02/13 20:05:14 gerd Exp $
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

:- module_interface(unify). /*%------------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.7 $}

This module provides a sound unification predicate |unify/2|.

\begin{BoxedSample}
[user 41]: unify(p(X,X),p(Z,g(Z))).

no (more) solution.
[user 42]: unify(p(X,X),p(Z,g(Y))).

X = g(Y)
Z = g(Y)
Y = Y
yes.
\end{BoxedSample}

\PL*/
:- export unify/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(unify).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate unify/2(Term1, Term2).

This predicate implements  sound unification. For  this purpose the \eclipse{}
build in is used. To do so the flag |occur_check| has to be turned on. The old
value is saved before to restore the proper value afterwards. For this purpose
the global variable |unify:occur_check| is used.

The  effect of using a	 sound	unification  algorithm	 is performed by   the
\eclipse{} system when the  predicate |unify/2| is  compiled into the code for
the  internal machine. This  is the only time  when  the flag |occur_check| is
taken into account. Changing it afterwards does not alter  the behavior of the
predicate.


\PL*/
:-	make_local_array('unify:occur_check'),
	get_flag(occur_check,OC),
	setval('unify:occur_check',OC),
	set_flag(occur_check,on).

unify(X,X).

:-	getval('unify:occur_check',OC),
	set_flag(occur_check,OC).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- skipped unify/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
