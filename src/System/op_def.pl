%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: op_def.pl,v 1.9 1995/04/06 12:53:14 gerd Exp $
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

:- module_interface(op_def). /*%-----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.9 $}

This module provides all operators needed by \ProCom. Things done here can be
separated into two groups. First, the redefinition of built-in operators and
second, the definition of new operators. Since we have to redefine and define
things only this module has no executable code in it. It is used by the
command 

| :- use_module(op_defs).|

With this instruction the module interface is loaded which contains all
definitions required.

First we redefine some build in operators. This is done mainly to
allow the definition of an operator with higher priority.
\PL*/
:-	op(1190, xfx, (:-)),
	op(1190, xfx, (?-)),
	op(1190, fx,  (?-)),
	op(1190, fx,  (if)),
	op(1190, xfx, (-->)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Now we can define our own operators.
\PL*/
:-	op(1200, xfx, (::)),  % Label separator
	op(1190, fx,  (:-)),  % Prefix if
	op(1190, fx,  (<-)),  % Prefix arrow
	op(1190, xfx, (<-)),  % Infix arrow
	op(800,	 fx,  (#)).   % Instruction operator
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(op_def).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
