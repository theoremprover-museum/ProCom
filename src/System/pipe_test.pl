%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%	Version:  1.00	 Date: 26/10/93	  File: pipe_test.pl 
%% Last Version:			  File:
%% Changes:
%% 26/10/93 Created
%%
%% Purpose:
%%
%% Author:  Gerd Neugebauer   
%%
%% Usage:   prolog pipe_test.pl 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: pipe_test.pl,v 1.2 1995/01/11 09:15:45 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- module(xxx).

:- get_flag(library_path,Path),
   set_flag(library_path,["/home/gerd/src/ProCom2/System"|Path]).
:- lib(pipe).
:- (current_macro(A,P,O,M),write(A-P-O-M),nl,fail;true).

:- define_pipe aha/1.

aha(Z): X ==> 32 where var(X).
aha(Z): inc(X) ==> X1 where number(X), X1 is X+1.


