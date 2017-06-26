/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: extension_procedure.pl,v 1.11 1995/05/15 19:58:27 gerd Exp $
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
\PL*/
:- module(extension_procedure).
:- compile(library(capri)).

info("$Revision: 1.11 $","The model elimination/extension procesdure.").

require_option(equality,[off]).
force_option('ProCom:automatic_put_on_path',[off]).

descriptor
	template(Pred,goal),
	template(-Pred,path).

descriptor
	template(Pred,goal),
	put_on_path(Pred),
	template(-Pred,extension).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
