%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: E_flatten.pl,v 1.7 1995/03/06 23:04:17 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% This file is part of ProCom.
%%% It is distributed under the GNU General Public License.
%%% See the file COPYING for details.
%%% 
%%% (c) Copyright 1995 Uwe Petermann
%%% 
%%% Net: uwe@imn.th-leipzig.de
%%% 
%%% Minor bug fixes by Gerd Neugebauer.
%%%****************************************************************************

:- module_interface('E_flatten'). /*%-----------------------------------------
\FileId{Uwe Petermann}{\RCSstrip$Revision: 1.7 $}

\PL*/
:- export 'E_flatten'/2.

:- begin_module('E_flatten').
info(filter,"$Revision: 1.7 $","Perform E-flattening acc. to Brand's method.").
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- lib(numbervars).

:-	op(1200, xfx,  (<-)).


'E_flatten'(Stream,OutStream) :-
	repeat,
	read(Stream,Term),
	( Term = end_of_file ->
	    true
	;   
	    flatten_clause(Term,Flat),
	    numbervars(Flat,0,_),
	    writeclause(OutStream,Flat),
	    fail
	),
	!.

flatten_file_to_file(Infile,Outfile) :-
	open(Infile,read,Stream),
	( Outfile = '' ->
	    'E_flatten'(Stream,output)
	;   open(Outfile,write,OutStream),
	    'E_flatten'(Stream,OutStream),
	    close(OutStream)
	),
	close(Stream).


eq_flatten_literal(Var,Goals,Var,Goals) :-
	var(Var),
	!.
eq_flatten_literal((A=B),In,(NA=NB),Goals) :-
	!,
	( var(A) ->
	    NB = A,
	    eq_flatten_literal(B,In,NA,Goals)
	; var(B) ->
	    NB = B,
	    eq_flatten_literal(A,In,NA,Goals)
	;
	    eq_flatten_literal(A,In,NA,Out),
	    eq_flatten_literal(B,Out,NB,Goals)
	).
eq_flatten_literal(Term,In,NewTerm,Out) :-
	nonvar(Term),
	functor(Term,F,A),
	( A=0 -> 
	   Term = NewTerm,
	   In = Out
	;  functor(NewTerm,F,A),
	   flatten_term(Term,In,NewTerm,Out,A)
	).

flatten_term(Var,Goals,Var,Goals) :-
	var(Var).
flatten_term(Term,In,Var,Out) :-
	nonvar(Term),
	functor(Term,F,A),
	( A = 0 ->
	    ( In = true ->
		Out = (Term=Var)
	    ;	Out = ((Term=Var),In)
	    )
%	    In = Out, 
%	    Term=Var
	;
	    functor(T,F,A),
	    ( In = true ->
		Goals = (T=Var)
	    ;	Goals = ((T=Var),In)
	    ),
	    flatten_term(Term,Goals,T,Out,A)
	).

flatten_term(Term,In,NewTerm,Out,Pos) :-
	( Pos < 1 ->
	    Out = In
	;
	    arg(Pos,Term,Arg),
	    arg(Pos,NewTerm,NewArg),
	    Pos1 is Pos - 1,
	    flatten_term(Arg,In,NewArg,NewGoals),
	    flatten_term(Term,NewGoals,NewTerm,Out,Pos1)
	).

flatten_clause((A:-B),(Fa:-Goals)) :-
	!,
	flatten__(A,Fa,true,G1),
	flatten__(B,Fb,true,G2),
	( G1 = true ->
	    ( G2 = true ->
		Goals = Fb
	    ;	Goals = (G2,Fb)
	    )
	;
	    ( G2 = true ->
		Goals = (G1,Fb)
	    ;	Goals = (G1,G2,Fb)
	    )
	).
flatten_clause((A <- B),(Fa:-Goals)) :-
	!,
	flatten__(A,Fa,true,G1),
	flatten__(B,Fb,true,G2),
	( G1 = true ->
	    ( G2 = true ->
		Goals = Fb
	    ;	Goals = (G2,Fb)
	    )
	;
	    ( G2 = true ->
		Goals = (G1,Fb)
	    ;	Goals = (G1,G2,Fb)
	    )
	).
flatten_clause((A , B),Result) :-
	!,
	flatten_clause((A ; B),Result).
flatten_clause((A ; B),Result) :-
	!,
	flatten__(A,Fa,true,G1),
	flatten__(B,Fb,true,G2),
	( G1 = true ->
	    ( G2 = true ->
		Result = (Fa;Fb)
	    ;	Result = ((Fa;Fb) :- G2)
	    )
	;
	    ( G2 = true ->
		Result = ((Fa;Fb) :- G1)
	    ;	Result = ((Fa;Fb) :- G1,G2)
	    )
	).
flatten_clause((:-B),(:- Goals)) :-
	!,
	flatten__(B,F,true,G),
	( G = true ->
	    Goals = F
	;
	    Goals = (G,F)
	).
flatten_clause(( <-(B)),(:- Goals)) :-
	!,
	flatten__(B,F,true,G),
	( G = true ->
	    Goals = F
	;
	    Goals = (G,F)
	).
flatten_clause((?-B),(?- Goals)) :-
	!,
	flatten__(B,F,true,G),
	( G = true ->
	    Goals = F
	;
	    Goals = (G,F)
	).
flatten_clause(Lit,(F:- Goals)) :-
	eq_flatten_literal(Lit,true,F,Goals).

flatten__((A;B),(FA;FB),Gin,G) :-
	!,
	flatten__(A,FA,Gin,G1),
	flatten__(B,FB,G1,G).
flatten__((A,B),(FA,FB),Gin,G) :-
	!,
	flatten__(A,FA,Gin,G1),
	flatten__(B,FB,G1,G).
flatten__(Lit,F,Gin,Goals) :-
	eq_flatten_literal(Lit,Gin,F,Goals).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
