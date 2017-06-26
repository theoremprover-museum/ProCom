/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: capri.pl,v 1.11 1995/05/15 19:58:27 gerd Exp $
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

\section{The CaPrI Loader}
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.11 $}

This is a simple file --- not a module --- it is used to define additional
provers.

We need some predicates from the \ProCom{} kernel. Thus we load it.
\PL*/
:- use_module(capricore).

:- lib(literal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Next we have to declare some of the keywords as dynamic. Thus it is possible
to make them optional. I.e. they need not to be given at all.

\PL*/
:- dynamic info/2,
	   force_option/2,
	   require_option/2,
	   requirement/1,
	   (require_predicate)/1,
	   (library_file)/1,
	   (library_path)/1,
	   (provide_definition)/1,
	   (start_descriptor)/1,
	   (end_descriptor)/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finally we allow the prover kernel to perform any actions necessary. This is
done by the tool |CaPrI Define Prover|. The current module name is
automatically provided by \eclipse.

\PL*/
:- 'CaPrI Define Prover'.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
