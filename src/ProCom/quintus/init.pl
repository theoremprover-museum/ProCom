%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: init.pl,v 1.3 1995/02/13 20:05:14 gerd Exp $
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
/*\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}

This library can be used to initialize the Prolog system. E.g. it might turn
off some debugging flags for efficiency reasons. For Quintus Prolog we turn
off the style checks to speed up compilation. The '~=' operator needs to be
defined.

Some compatibility predicates are defined as well.
\PL*/
:- op(700, xfx, ('~=') ).
:- no_style_check(all).
once(Goal) :- call(Goal),!.
nonground(Term) :- \+ ground(Term).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
