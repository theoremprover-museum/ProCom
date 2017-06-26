%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: nf_anti_prenexing.pl,v 1.3 1995/04/24 21:29:11 gerd Exp $
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

:- module_interface(nf_anti_prenexing). /*%------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.3 $}

\PL*/
:- export anti_prenexing/2.
:- begin_module(nf_anti_prenexing).

:-	lib(lists),
	lib(eq_member),
	lib(nf_intern).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate or_member/3(Formula,Member,Rest).

\PL*/
or_member(true,true,false).
or_member(false,false,false).
or_member(not(P),not(P),false).
or_member(predicate(P),predicate(P),false).
or_member(exists(V,F),exists(V,F),false).
or_member(forall(V,F),forall(V,F),false).
or_member((A implies B),(A implies B),false).
or_member((A iff B),(A iff B),false).
or_member((A and B),(A and B),false).
or_member((A or B),A,B).
or_member((A or B),M,Rest) :-
	or_member(B,M,R),
	( R == false ->
	    Rest = A
	;   Rest = (A or R)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate and_member/3(Formula,Member,Rest).

\PL*/
and_member(true,true,true).
and_member(false,false,true).
and_member(not(P),not(P),true).
and_member(predicate(P),predicate(P),true).
and_member(exists(V,F),exists(V,F),true).
and_member(forall(V,F),forall(V,F),true).
and_member((A implies B),(A implies B),true).
and_member((A iff B),(A iff B),true).
and_member((A or B),(A or B),true).
and_member((A and B),A,B).
and_member((A and B),M,Rest) :-
	and_member(B,M,R),
	( R == true ->
	    Rest = A
	;   Rest = (A and R)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate anti_prenexing/2(+Formula, ?Transformed).

This predicate is used to minimize the number of quantifiers in |Formula| and
to push them inwards. The formula |Formula| is assumed to have negation signs
before predicates only. The result is unified with |Transformed|.

The predicate has to look at the leading operator of |Formula|. All
possibilities of operators have to be considered. In the simple cases nothing
has to be done and the formula is returned unchanged.


\PL*/
anti_prenexing(true,true).
anti_prenexing(false,false).
anti_prenexing(predicate(P),predicate(P)).
anti_prenexing(not(P),not(P1)) :-
	anti_prenexing(P,P1).
anti_prenexing((A or B),Result) :-
	( A = exists(X,FA) ->
	    ( or_member(B,exists(X,FB),Etc) ->
		disjunction(exists(X,(FA or FB)),Etc,Formula),
		anti_prenexing(Formula,Result)
	    ; or_member(B,exists(Y,FB),Etc) ->
		Length_X is length(X),
		Length_Y is length(Y),
		( (Length_X > Length_Y) ->
		    append(X,XR,Y),
		    disjunction(exists(Y,(exists(XR,FA) or FB)),Etc,Formula)
		;   append(Y,XR,X),
		    disjunction(exists(X,(FA or exists(XR,FB))),Etc,Formula)
		),
		anti_prenexing(Formula,Result)
	    ;
		anti_prenexing(A,AA),
		anti_prenexing(B,AB),
		Result = (AA or AB)
	    )
	;
	    anti_prenexing(A,AA),
	    anti_prenexing(B,AB),
	    Result = (AA or AB)
	).
anti_prenexing((A and B),Result) :-
	( A = forall(X,FA) ->
	    ( and_member(B,forall(X,FB),Etc) ->
		conjunction(forall(X,(FA and FB)),Etc,Formula),
		anti_prenexing(Formula,Result)
	    ; and_member(B,forall(Y,FB),Etc) ->
		Length_X is length(X),
		Length_Y is length(Y),
		( (Length_X > Length_Y) ->
		    append(X,XR,Y),
		    conjunction(forall(Y,(forall(XR,FA) and FB)),Etc,Formula)
		;   append(Y,XR,X),
		    conjunction(forall(X,(FA and forall(XR,FB))),Etc,Formula)
		),
		anti_prenexing(Formula,Result)
	    ;
		anti_prenexing(A,AA),
		anti_prenexing(B,AB),
		Result = (AA and AB)
	    )
	;
	    anti_prenexing(A,AA),
	    anti_prenexing(B,AB),
	    Result = (AA and AB)
	).
anti_prenexing(exists(X,F1),Result) :-
	anti_prenexing(F1,F2),
	( F2 = exists(X3,F3) ->
	    eq_union(X,X3,Vars),
	    Result = exists(Vars,F3)
	;   Result = exists(X,F2)
	).
anti_prenexing(forall(X,F1),Result) :-
	anti_prenexing(F1,F2),
	( F2 = forall(X3,F3) ->
	    eq_union(X,X3,Vars),
	    Result = forall(Vars,F3)
	;   Result = forall(X,F2)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate cluster_subformulae/3().

\PL*/
cluster_subformulae(Cand,X,Key) :-
	member(C,Cand),
	term_variables(C,Vars),
	eq_intersect(X,Vars,V),
	copy_term(V-C,Key).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate join_quantifiers/2().

\PL*/
join_quantifiers(true,true).
join_quantifiers(false,false).
join_quantifiers(predicate(P),predicate(P)).
join_quantifiers(not(P),not(P1)) :-
	join_quantifiers(P,P1).
join_quantifiers((A or B),Result) :-
	( A = exists(X,FA) ->
	    ( or_member(B,exists(X,FB),Etc) ->
		disjunction(exists(X,(FA or FB)),Etc,Formula),
		join_quantifiers(Formula,Result)
	    ; or_member(B,exists(Y,FB),Etc) ->
		Length_X is length(X),
		Length_Y is length(Y),
		( (Length_X > Length_Y) ->
		    append(X,XR,Y),
		    disjunction(exists(Y,(exists(XR,FA) or FB)),Etc,Formula)
		;   append(Y,XR,X),
		    disjunction(exists(X,(FA or exists(XR,FB))),Etc,Formula)
		),
		join_quantifiers(Formula,Result)
	    ;
		join_quantifiers(A,AA),
		join_quantifiers(B,AB),
		Result = (AA or AB)
	    )
	;
	    join_quantifiers(A,AA),
	    join_quantifiers(B,AB),
	    Result = (AA or AB)
	).
join_quantifiers((A and B),Result) :-
	( A = forall(X,FA) ->
	    ( and_member(B,forall(X,FB),Etc) ->
		conjunction(forall(X,(FA and FB)),Etc,Formula),
		join_quantifiers(Formula,Result)
	    ; and_member(B,forall(Y,FB),Etc) ->
		Length_X is length(X),
		Length_Y is length(Y),
		( (Length_X > Length_Y) ->
		    append(X,XR,Y),
		    conjunction(forall(Y,(forall(XR,FA) and FB)),Etc,Formula)
		;   append(Y,XR,X),
		    conjunction(forall(X,(FA and forall(XR,FB))),Etc,Formula)
		),
		join_quantifiers(Formula,Result)
	    ;
		join_quantifiers(A,AA),
		join_quantifiers(B,AB),
		Result = (AA and AB)
	    )
	;
	    join_quantifiers(A,AA),
	    join_quantifiers(B,AB),
	    Result = (AA and AB)
	).
join_quantifiers(exists(X,F1),Result) :-
	join_quantifiers(F1,F2),
	( functor(F2,and,2) ->
	    conj_2_list(F2,LF2),
	    setof(KP,join_quantifiers__(LF2,X,KP),Conj),
	    join_quantifiers_join(Conj,none,[],List),
	    ( List == Conj ->
		Result = exists(X,F2)
	    ;
		findall(Formula,
			( member(V-F,List),
			  list_2_conj(F,C),
			  ( V == [] ->
			      Formula = C
			  ;   Formula = exists(V,C) 
			  )
			),
			Res),
		list_2_conj(Res,Res2),
		join_quantifiers(Res2,Result)
	    )
	; functor(F2,or, 2) ->
	    disj_2_list(F2,LF2),
	    setof(KP,join_quantifiers__(LF2,X,KP),Conj),
	    join_quantifiers_join(Conj,none,[],List),
	    ( List == Conj ->
		Result = exists(X,F2)
	    ;
		findall(Formula,
			( member(V-F,List),
			  list_2_disj(F,C),
			  ( V == [] ->
			      Formula = C
			  ;   Formula = exists(V,C) 
			  )
			),
			Res),
		list_2_disj(Res,Res2),
		join_quantifiers(Res2,Result)
	    )
	; F2 = exists(X3,F3) ->
	    eq_union(X,X3,Vars),
	    Result = exists(Vars,F3)
	;   Result = exists(X,F2)
	).
join_quantifiers(forall(X,F1),Result) :-
	join_quantifiers(F1,F2),
	( functor(F2,and,2) ->
	    conj_2_list(F2,LF2),
	    setof(KP,join_quantifiers__(LF2,X,KP),Conj),
	    join_quantifiers_join(Conj,none,[],List),
	    ( List == Conj ->
		Result = exists(X,F2)
	    ;
		findall(Formula,
			( member(V-F,List),
			  list_2_conj(F,C),
			  ( V == [] ->
			      Formula = C
			  ;   Formula = forall(V,C) 
			  )
			),
			Res),
		list_2_conj(Res,Res2),
		join_quantifiers(Res2,Result)
	    )
	; functor(F2,or, 2) ->
	    disj_2_list(F2,LF2),
	    setof(KP,join_quantifiers__(LF2,X,KP),Conj),
	    join_quantifiers_join(Conj,none,[],List),
	    ( List == Conj ->
		Result = exists(X,F2)
	    ;
		findall(Formula,
			( member(V-F,List),
			  list_2_disj(F,C),
			  ( V == [] ->
			      Formula = C
			  ;   Formula = forall(V,C) 
			  )
			),
			Res),
		list_2_disj(Res,Res2),
		join_quantifiers(Res2,Result)
	    )
	; F2 = forall(X3,F3) ->
	    eq_union(X,X3,Vars),
	    Result = forall(Vars,F3)
	;   Result = forall(X,F2)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate anti_prenexing_join/4().

\PL*/
anti_prenexing_join([],_,[],[]).
anti_prenexing_join([W-F|T],V,L,Result) :-
	( V = W ->
	    Result = R,
	    L = [F|L2],
	    anti_prenexing_join(T,V,L2,R)
	;   L = [],
	    Result = [(W-[F|L2])|R],
	    anti_prenexing_join(T,W,L2,R)
	).


:- skipped and_member/3, or_member/3.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog */
