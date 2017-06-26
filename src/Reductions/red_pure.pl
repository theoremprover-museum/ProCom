%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: red_pure.pl,v 1.6 1995/01/27 13:45:38 gerd Exp $
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

:- module_interface(red_pure). /*%-------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.6 $}

The module {\sf \PrologFILE} exports the following predicates:

\PL*/
:- export red_pure/0,
          red_pure/1,
          red_pure/2.

:- begin_module(red_pure).
info(reduction,"$Revision: 1.6 $","").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-      lib(matrix).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The traditional version of the purity reduction reads as follows.

\begin{description}
  \item [pure]
        Let $L$\/ be a literal which has no connection and $C$\/ the clause
        containing $L$\/. Then $C$\/ can be deleted.
\end{description}

This version has to be lifted to take into account the reachability graph. For
this purpose we switch to the level of contrapositives instead of clauses. In
this case purity may occur in the head or in the body of a contrapositive.
Accordingly the adapted definition considers these two cases.

\begin{description}
  \item [pure] Let $L$\/ be a literal in the contrapositive $C$.
  $L$\/ is pure if
  \begin{itemize}
    \item $L$\/ is the entry literal of $C$\/ and there is no arc in the
    reachability graph going into $L$.
    \item $L$\/ is a body literal of $C$\/ and there is no arc in the
    reachability graph going out of $L$.
  \end{itemize}
  Any contrapositive containing a pure literal can be deleted.
\end{description}

According to the construction of the reachability graph the first case
corresponds to contrapositives which are not reachable. Thus they can not
contribute to a proof.

The second case corresponds to a contrapositive where a body literal is
equivalent to false. In this case this contrapositive can not contribute to a
proof. 




\PL*/
pure_p(ClauseIndex,LiteralIndex,_) :-
        \+ 'GoalClause'(ClauseIndex),
        \+ 'Connection'(_,_,ClauseIndex,LiteralIndex).
pure_p(_,_,Body) :-
        member(literal(_,ClauseIndex-LiteralIndex),Body),
        \+ 'Connection'(ClauseIndex,LiteralIndex,_,_).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate red_pure/0().

This predicate searches the matrix for pure literals. The contrapositives
containing pure literals are deleted. The predicate uses a failure driven
loop.

\PL*/
red_pure :-
        (   'Clause'(_,Body,ClauseIndex-LiteralIndex),
            pure_p(ClauseIndex,LiteralIndex,Body),
            delete_contrapositive(ClauseIndex,LiteralIndex),
            fail
        ;
            true
        ).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate red_pure/1(-Deleted).

This predicate searches the matrix for pure literals. The contrapositives
containing pure literals are deleted. The predicate returns the list of
deleted clauses |Deleted|.

\PL*/
red_pure(Deleted) :-
        findall(ClauseIndex-LiteralIndex,
                (   'Clause'(_,Body,ClauseIndex-LiteralIndex),
                    pure_p(ClauseIndex,LiteralIndex,Body),
                    delete_contrapositive(ClauseIndex,LiteralIndex)
                ),
                Deleted).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate red_pure/2(+Candidates,-Deleted).

This predicate searches the matrix for pure literals, but only the
contrapositives |Candidates| are taken into account. The contrapositives
containing pure literals are deleted. The predicate returns the list of
deleted contrapositives |Deleted|.

\PL*/
red_pure(Candidates,Deleted) :-
        findall(ClauseIndex-LiteralIndex,
                (   member(ClauseIndex-LiteralIndex,Candidates),
                    'Clause'(_,Body,ClauseIndex-LiteralIndex),
                    pure_p(ClauseIndex,LiteralIndex,Body),
                    delete_contrapositive(ClauseIndex,LiteralIndex)
                ),
                Deleted).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */


