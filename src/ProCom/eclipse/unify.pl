%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: unify.pl,v 1.7 1994/12/12 17:07:16 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1994 Gerd Neugebauer
%%% 
%%% Net: gerd@imn.th-leipzig.de
%%% 
%%%****************************************************************************
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.7 $}

This library provides a predicate which unifies its arguments. A sound
unification algorithm is used.

\Predicate unify/2(Term1,Term2).

The kind of unification algorithm used by \eclipse is determined at compile
time of a predicate. For this purpose the flag |occur_check| is taken into
account. Once a clause is compiled, the kind of unification can no longer be
influenced (except by redefining the predicate).

We use the trick to retrieve the value of the flag |occur_check| and store it
in a global variable of the same name. Then we define turn on the occur check
and define the unify/2 predicate. Finally re restore the old value of the flag
|occur_check|.

\PL*/
:-      get_flag(occur_check,OC),
        setval(occur_check,OC),
        set_flag(occur_check,on).
unify(X,X).
:-      getval(occur_check,OC),
        set_flag(occur_check,OC).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
