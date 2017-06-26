%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: matrix.pl,v 1.28 1995/05/15 19:58:27 gerd Exp $
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

:- module_interface(matrix). /*%-----------------------------------------------
\FileId{Gerd Neugebauer}{\RCSstrip$Revision: 1.28 $}

\input{../Doc/matrix.tex}

\PL*/
:- export 'Clause'/1,
	  'Clause'/2,
	  'ClauseLiteral'/2,
	  'ClauseLength'/2,
	  'Connection'/2,
	  'Contrapositive'/3,
	  'GoalClause'/1,
	  'Label'/2,
	  'NegativeClause'/1,
	  'PositiveClause'/1,
	  add_clause/1,
	  add_connection/2,
	  add_contrapositive/3,
	  add_goal_label/1,
	  add_label/1,
	  add_label/2,
	  add_resolvent/3,
	  delete_clause/1,
	  delete_contrapositive/1,
	  delete_connection/2,
	  delete_goal_label/1,
	  delete_label/2,
	  delete_literal/1,
	  delete_matrix/0,
	  new_clause_index/1,
	  new_literal_index/2,
	  show_contrapositives/2,
	  show_matrix/2.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- begin_module(matrix).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	lib(hook),
	lib(message),
	lib(literal),
	lib(numbervars).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	set_flag(occur_check,on).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:- define_hook reset_matrix_hook/0.
:- define_hook delete_clause_hook/1.
:- define_hook delete_contrapositive_hook/1.
:- define_hook delete_literal_hook/1.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate macro_make_key/2(Macro,Replacement).

This predicate performs the translation of certain terms at read time. This
way the efficiency is combined with readability.

\PL*/
macro_make_key(no_macro_expansion(make_pred_key(Sign,Functor,Arity,Key)),
	concat_atom(['#predicate ',Sign,Functor,'/',Arity],Key)).
macro_make_key(no_macro_expansion(make_index_key(Clause,Literal,Key)),
	concat_atom(['#index ',Clause,'-',Literal],Key)).
macro_make_key(no_macro_expansion(make_clause_key(Clause,Key)),
	concat_atom(['#clause ',Clause],Key)).
macro_make_key(no_macro_expansion(make_length_key(Clause,Key)),
	concat_atom(['#length ',Clause],Key)).
macro_make_key(no_macro_expansion(make_c_to_key(C-I,Key)),
	concat_atom(['#connection -> ',C,'-',I],Key)).
macro_make_key(no_macro_expansion(make_c_from_key(C-I,Key)),
	concat_atom(['#connection <- ',C,'-',I],Key)).

:- define_macro(make_pred_key/4,macro_make_key/2,[]).
:- define_macro(make_index_key/3,macro_make_key/2,[]).
:- define_macro(make_clause_key/2,macro_make_key/2,[]).
:- define_macro(make_length_key/2,macro_make_key/2,[]).
:- define_macro(make_c_to_key/2,macro_make_key/2,[]).
:- define_macro(make_c_from_key/2,macro_make_key/2,[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
:-	make_local_array('Max Clause'),
	setval('Max Clause',0).
:-	make_local_array(last_clause),
	setval(last_clause,'').
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Contrapositive/3(?Literal, ?Body, ?Index).

This predicate can be used to get the complete information about a
contrapositive in the matrix.\footnote{To be precise we have to note that
  different entry points to a clause may keep different informations.} This
predicate assumes that either the index |Index| is ground or the literal
|Literal| is sufficiently instanciated. In the first case at most one solution
is returned. In the second case all solutions are returned upon backtracking.
If no neither the literal nor the index is given all contrapositives are
enumerated. This is the worst case and should be avoided.

|Literal| is unified with a literal (in internal representation; see
\ref{matrix:data.structures}). |Index| is a literal index. |Body| is a list
containing elements of the form {\em literal(L,I)}\/ where {\em L}\/ is a
literal and {\em I}\/ is an index. |Body| contains all literals which are in
the contrapositive of |Literal| except |Literal| itself. The complete clause
represented by this contrapositive would be
\begin{BoxedSample}
  [ literal(Literal,Index) \char"7C Body ]
\end{BoxedSample}%"

Let us assume that the matrix given earlier is stored in the module {\sf
  matrix}. Then we can access it as shown in the next example.
\begin{BoxedSample}
  ?- 'Contrapositive'(q(A,B),Body,Index).
  A = a
  B = X
  Body = []
  Index = 2-1
  ;
  A = X
  B = Y
  Body = [literal(--q(f(X),Y),3-2)]
  Index = 3-1
  ;
  No more solution
\end{BoxedSample}

\PL*/
'Contrapositive'(Literal, Body, C-I) :-
	atomic(C),
	atomic(I),
	!,
	make_index_key(C,I,Key),
	recorded(Key,contrapositive(Literal,Body,C-I)).
'Contrapositive'(Literal, Body, C-I) :-
	nonvar(Literal),
	Literal =.. [Sign,Pred],
	nonvar(Pred),
	!,
	functor(Pred,F,A),
	make_pred_key(Sign,F,A,Key),
	recorded(Key,contrapositive(Literal,Body,C-I)).
'Contrapositive'(Literal, Body, C-I) :-
	recorded('#index',C-I),
	make_index_key(C,I,Key),
	recorded(Key,contrapositive(Literal,Body,C-I)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Clause/1(?ClauseIndex).

This predicate unifies |ClauseIndex| with each index of a clause currently
stored in the module matrix in turn.
\begin{BoxedSample}
  ?- findall( CI, 'Clause'(CI), ClauseList ).
  ClauseList = [1,2,3,4]
\end{BoxedSample}

\PL*/
'Clause'(ClauseIndex) :-
	recorded('#clause',ClauseIndex).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Clause/2(?ClauseIndex, ?Body).

This predicate takes a clause index |ClauseIndex| and unifies |Body| with a
list of the form {\em literal(L,I)}\/ where {\em L}\/ is a literal and {\em
  I}\/ is an index. Body contains all literals left in the specified clause.

The literals in the clause are not modified when a single contrapositive is
changed. Only the deletion of a literal in a clause, building the resolvent,
or deleting a complete clause may change the information returned by this
predicate.

Upon backtracking all unifiable pairs of clause index and literal list are
returned.
\begin{BoxedSample}
  ?- 'Clause'(3,Body).
  Body = [literal(++q(X,Y),3-1),literal(--q(f(X),Y),3-2)]
\end{BoxedSample}

\PL*/
'Clause'(ClauseIndex,Body) :-
	recorded('#Clause',ClauseIndex=Body).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate ClauseLiteral/2(?ClauseIndex, ?Index).

This predicate establishes a relation between the clause and literal indices.
It can be used to find one or all literals in a clause as well as to find the
clause index of an index.

\begin{BoxedSample}
  ?- 'ClauseLiteral'(1,Index).
  Index = 1-1
  ;
  Index = 1-2
  ;
  No more solution

  ?- 'ClauseLiteral'(CI,3-2).
  CI = 3
\end{BoxedSample}

\PL*/
'ClauseLiteral'(ClauseIndex,ClauseIndex-Index) :-
	recorded('#index',ClauseIndex-Index).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate PositiveClause/1(?ClauseIndex).

This predicate unifies the clause index |ClauseIndex| with those clauses which
contain positive clauses only. All literals are considered which are currently
in the clause. Even so a single contrapositive may or may not be positive the
whole clause can have an independent property.
\begin{BoxedSample}
  ?- 'PositiveClause'(CI).
  CI = 2
\end{BoxedSample}

\PL*/
'PositiveClause'(ClauseIndex) :-
	recorded('#positive_clause',ClauseIndex).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate NegativeClause/1(?ClauseIndex).

This predicate unifies the clause index |ClauseIndex| with those clauses which
contain negative clauses only. All literals are considered which are currently
in the clause. Even so a single contrapositive may or may not be negative the
whole clause can have an independent property.
\begin{BoxedSample}
  ?- 'NegativeClause'(CI).
  CI = 4
\end{BoxedSample}

\PL*/
'NegativeClause'(ClauseIndex) :-
	recorded('#negative_clause',ClauseIndex).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate ClauseLength/2(?ClauseIndex,?Length).

This predicate enumerates all pairs of clause indices |ClauseIndex| and the
length |Length| of the clause belonging to |ClauseIndex|. This information is
extracted for the complete clause. Single contrapositives may have a different
length.

\begin{BoxedSample}
  ?- 'ClauseLength(1,Len).
  Len = 2
\end{BoxedSample}

\PL*/
'ClauseLength'(ClauseIndex,Length) :-
	recorded('#clause',ClauseIndex),
	make_length_key(ClauseIndex,Key),
	recorded(Key,Length).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Label/2(?ClauseIndex, ?Label).

This predicate enumerates all pairs consisting of a clause index |ClauseIndex|
and an associated label |Label|. Note that the goal label is a special kind of
label which is not returned with this predicate.
\begin{BoxedSample}
  ?- 'Label'(CI,Label).
  CI = 2
  Label = base
  ;
  CI = 3
  Label = loop
  ;
  No more solution
\end{BoxedSample}

\PL*/
'Label'(ClauseIndex,Label) :-
	recorded('#label',ClauseIndex=Label).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate GoalClause/1(?ClauseIndex).

This predicate unifies |ClauseIndex| with the index of each clause which is
marked as goal. Several solutions can be returned upon backtracking.
\begin{BoxedSample}
  ?- findall( CI, 'GoalClause'(CI), ClauseList ).
  ClauseList = [4]
\end{BoxedSample}

\PL*/
'GoalClause'(ClauseIndex) :-
	recorded('#goal',ClauseIndex).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate Connection/2(?FromIndex, ?ToIndex).

This predicate relates the indices |FromIndex| and |ToIndex| according to the
connectedness. The connection graph contains an arc from a literal $L$\/ to
another literal $L'$\/ if an extension or reduction from $L$\/ to $L'$\/ can
be part of the final proof. This information is provided by the module {\sf
  connection\_graph}. 

\PL*/	
'Connection'(C-I,To) :-
	atomic(C),
	atomic(I),
	!,
	make_c_to_key(C-I,Key),
	recorded(Key,To).
'Connection'(From,C-I) :-
	atomic(C),
	atomic(I),
	!,
	make_c_from_key(C-I,Key),
	recorded(Key,From).
'Connection'(From,To) :-
	recorded('#index',From),
	make_c_to_key(From,Key),
	recorded(Key,To).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate new_clause_index/1(-ClauseIndex).

This predicate unifies |ClauseIndex| with a unique new clause index. This
predicate is not resatisfiable.
\begin{BoxedSample}
  ?- new\_clause\_index(CI).
  CI = 5
\end{BoxedSample}

\PL*/
new_clause_index(ClauseIndex) :-
	incval('Max Clause'),
	getval('Max Clause',ClauseIndex).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate new_literal_index/1(+ClauseIndex,-Index).

This predicate unifies |Index| with a unique new index in the clause
|ClauseIndex|. The index is not reserved in any way. If no associated literal
is added then the new index will be returned upon the next invocation of
|new_literal_index/1| again. This predicate is not resatisfiable.
\begin{BoxedSample}
  ?- new\_literal\_index(3,I).
  I = 3-3
\end{BoxedSample}

\PL*/
new_literal_index(ClauseIndex,ClauseIndex-Index) :-
	atomic(ClauseIndex),
	'ClauseLength'(ClauseIndex,Length),
	get_number(Length,Index),
	\+ recorded('#index',ClauseIndex-Index),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate get_number/2(From, Value).

This predicate generates all integers which are greater or equal to the
initial limit |From|. The result is unified with |Value|. This predicate has
infinitly many solutions.

\PL*/
get_number(Start,Start).
get_number(Start,N) :-
	S is Start + 1,
	get_number(S,N).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_clause/1(+Literals).

This predicate takes a list of literals |Literals| and stores all
contrapositives in the module {\sf matrix}. The hook |add_clause_hook/2| is
evaluated with the index of the new clause and the list of literals |Literals|
as arguments. This hook is evaluated after the literals have been added.

The list of literals |Literals| can be preceded by the goal label |?-|. In
this case the goal label is set also for this clause and the hook
|add_goal_clause_hook/1| is evaluated at the end. The argument is the index of
the goal clause.

The following instructions add all clauses to the module {\sf
  matrix}. Nevertheless the labels and the connections are not considered yet.
\begin{BoxedSample}
  ?- add\_clause([++p(X),--q(f(X),g(X)]).
  ?- add\_clause([++q(a,X)]).
  ?- add\_clause([++q(X,Y),--q(f(X),Y)]).
  ?- add\_clause((?-[--p(Z)])).
\end{BoxedSample}

\PL*/
add_clause(?-List) :-
	!,
	new_clause_index(CI),
	add_clause(List,CI),
	add_goal_label(CI).
add_clause(List) :-
	new_clause_index(CI),
	add_clause(List,CI).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_clause/2(+List, +ClauseIndex).

\PL*/
add_clause(List,CI) :-
	record('#clause',CI),
	length(List,Length),
	make_length_key(CI,Key),
	record(Key,Length),
	enumerate_clause(List,CI,1,LitList),
	record('#Clause',CI=LitList),
	(   delete(literal(Literal,Index),LitList,Body),
	    add_contrapositive(Literal,Body,Index),
	    fail
	;   true
	),
	( member(--_,List) ->
	    true
	;   record('#positive_clause',CI)
	),
	( member(++_,List) ->
	    true
	;   record('#negative_clause',CI)
	),
	setval(last_clause,CI),
	run_hooks(add_clause_hook,[CI,List]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate enumerate_clause/4(Literals,Clause,No,LiteralList).

\PL*/
enumerate_clause([],_,_,[]).
enumerate_clause([H|T],C,N,[literal(H,C-N)|T2]) :-
	N1 is N + 1,
	enumerate_clause(T,C,N1,T2).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_contrapositive/3(+Literal, +Body, +Index).

This predicate adds the contrapositive with the given information to the
matrix. The entry point of the contrapositive is the literal |Literal| which
has the index |Index|. |Body| is a list of elements of the form {\em
  literal(L,I)}\/ where {\em L}\/ is a literal and {\em I}\/ is its index. The
literals of |Body| are the literals in the contrapositive except |Literal|.

\begin{BoxedSample}
  ?- add\_contrapositive(++p(X),[literal(--q(f(X),g(X)),1-2)],1-1).
\end{BoxedSample}

\PL*/
add_contrapositive(Literal,Body,C-I) :-
	nonvar(Literal),
	Literal =.. [Sign,Pred],
	nonvar(Pred),
	atomic(C),
	atomic(I),
	!,
	functor(Pred,F,A),
	make_pred_key(Sign,F,A,Key),
	record(Key,contrapositive(Literal,Body,C-I)),
	make_index_key(C,I,IKey),
	record(IKey,contrapositive(Literal,Body,C-I)),
	( recorded('#index',C-I) ->
	    true
	;   record('#index',C-I)
	).
add_contrapositive(Literal,Body,C-I) :-
	err("*** Type error in ",
	    add_contrapositive(Literal,Body,C-I),
	    "\n*** The literals are expected to be of the form ++P or --P."),
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_connection/2(+FromIndex, +ToIndex).

This predicate adds a connection from the literal |FromIndex| to the literal
|ToIndex|. If such a connection already exists then this predicate simply
succeeds. The arguments have to be ground.
\begin{BoxedSample}
  ?- add\_connection(4-1,1-1).
\end{BoxedSample}

\PL*/
add_connection(A-B,C-D) :-
	atomic(A),
	atomic(B),
	atomic(C),
	atomic(D),
	!,
	make_c_to_key(A-B,KeyAB),
	( recorded(KeyAB,C-D) ->
	    true
	;   record(KeyAB,C-D)
        ),
	make_c_from_key(C-D,KeyCD),
	( recorded(KeyCD,A-B) ->
	    true
	;   record(KeyCD,A-B)
	).
add_connection(A-B,C-D) :-
	err("*** Instanciation error in ",add_connection(A-B,C-D)),
	fail.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_goal_label/1(+ClauseIndex).

This predicate marks the clause with the index |ClauseIndex| as goal. {\em
  No}\/ implicit negation is performed! If the given clause has already a goal
label then the predicate simply succeeds.

The following example adds the goal label to each negative clause.
\begin{BoxedSample}
  ?- 'NegativeClause'(NC), add\_goal\_label(NC), fail; true.
\end{BoxedSample}

\PL*/
add_goal_label([]) :- !.
add_goal_label([H|T]) :-
	!,
	add_goal_label(H),
	add_goal_label(T).
add_goal_label(ClauseIndex) :-
	atomic(ClauseIndex),
	( recorded('#goal',ClauseIndex) ->
	    true
	;   record('#goal',ClauseIndex)
        ),
	run_hooks(add_goal_label_hook,[ClauseIndex]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate add_label/2(+ClauseIndex, +Label).

This predicate adds the label |Label| to the clause with the index
|ClauseIndex|. If the label already exists the predicate simply succeeds.

The following goal may be called from within the parser to store the label of
the third clause.
\begin{BoxedSample}
  ?- add\_label(3,loop).
\end{BoxedSample}

\PL*/
add_label(ClauseIndex,Label) :-
	atomic(ClauseIndex),
	( recorded('#label',(ClauseIndex=Label)) ->
	    true
	;   record('#label',(ClauseIndex=Label))
	),
	run_hooks(add_label_hook,[ClauseIndex,Label]).

add_label(Label) :-
	getval(last_clause,CI),
	add_label(CI,Label).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^





\PL*/
add_resolvent(I1,I2,clause(CI)) :-
	var(CI),
	make_resolvent(I1,I2,List),
	new_clause_index(CI),
	add_clause(List,CI),
	run_hooks(add_resolvent_hook,[clause(CI)]).
add_resolvent(I1,I2,clause(CI)) :-
	nonvar(CI),
	make_resolvent(I1,I2,List),
	delete_clause(CI),
	add_clause(List,CI),
	run_hooks(add_resolvent_hook,[clause(CI)]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_resolvent/3(I1,I2,List).

pos/neg must be updated!!!

\PL*/
make_resolvent(I1,I2,List) :-
	'Contrapositive'(Lit1,Body1,I1),
	'Contrapositive'(Lit2,Body2,I2),
	negate_literal(Lit1,NegLit1),
	Lit2 = NegLit1,
	strip_literals(Body1,List,X),
	strip_literals(Body2,X,[]).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate strip_literals/3(List,New,Tail).

\PL*/
strip_literals([],X,X).
strip_literals([literal(Lit,_)|T],[Lit|D],L) :-
	strip_literals(T,D,L).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


\Predicate delete_matrix/0().

This predicate removes all information stored in the module {\sf matrix}.
Afterwards the information is no longer accessible. Before the matrix is
cleared the hook |delete_matrix_hook/0| is evaluated. At this time the matrix
is still intact. Afterwards the clauses are deleted. This may lead to the
execution of the hook |delete_clause/1|. See the documentation of the
predicate |delete_clause_hook/1| for details.
\begin{BoxedSample}
  ?- delete\_matrix.
\end{BoxedSample}

\PL*/
delete_matrix :-
	run_hooks(delete_matrix_hook),
	setval('Max Clause',0),
	repeat,	\+ delete_clause(_),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_clause/1(?ClauseIndex).

This predicate deletes all clauses --- upon backtracking --- for which the
clause index unifies with |ClauseIndex|.

All information is erased having any relation to the clause being removed.

The hook |delete_clause_hook/1| is evaluated for each clause removed. The
argument is the instantiated clause index of the clause being deleted. The
hook is called before any other information about this clause is removed from
matrix. 

The following example deletes the clause with the index 3.
\begin{BoxedSample}
  ?- delete\_clause(3).
\end{BoxedSample}

\PL*/	
delete_clause(ClauseIndex) :-
	recorded('#clause',ClauseIndex),
	once(delete_clause__(ClauseIndex)).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\PL*/
delete_clause__(ClauseIndex) :-
	run_hooks(delete_clause_hook,[ClauseIndex]),
	erase('#clause',ClauseIndex),
	erase('#Clause',(ClauseIndex=_)),
	repeat,	\+ erase('#goal',ClauseIndex),
	repeat,	\+ erase('#label',ClauseIndex),
	repeat,	\+ erase('#positive_clause',ClauseIndex),
	repeat,	\+ erase('#negative_clause',ClauseIndex),
	(   recorded('#index',ClauseIndex-Index),
	    erase('#index',ClauseIndex-Index),
	    make_index_key(ClauseIndex,Index,IKey),
	    ( erase(IKey,contrapositive(Literal,_,ClauseIndex-Index)) -> 
		Literal =.. [Sign,Pred],
		functor(Pred,F,A),
		make_pred_key(Sign,F,A,Key),
		erase(Key,contrapositive(_,_,ClauseIndex-Index))
	    ),
	    fail		    
	;   true
	),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_contrapositive/1(?Index).

This predicate deletes a single contrapositive identified by |Index|. |Index|
may be un-instantiated in which case all contrapositives are removed upon
backtracking. The connections starting from the literal |Index| are also
removed.

The clause as a whole is not affected. Nor are the |ClauseLength| or other
informations related to the whole clause. As a consequence the clause can
continue to exist even so there are no contrapositives left for it.

The hook |delete_contrapositive_hook/1| is evaluated for each contrapositive
being removed. The hook is called before the information is erased from the
matrix. The argument of the hook is the instantiated index of the
contrapositive being deleted.

\begin{BoxedSample}
  ?- delete\_contrapositive(3-2).
\end{BoxedSample}

\PL*/
delete_contrapositive(C-I) :-
	recorded('#index',C-I),
	run_hooks(delete_contrapositive_hook,[C-I]),
	make_index_key(C,I,Key),
	( erase(Key,contrapositive(Literal,_Body,C-I)) ->
	    Literal =.. [Sign,Pred],
	    functor(Pred,F,A),
	    make_pred_key(Sign,F,A,Key2),
	    erase(Key2,contrapositive(_,_,C-I))
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_connection/2(?FromIndex, ?ToIndex).

This predicate deletes upon backtracking all connections for which the
starting point unifies with |FromIndex| and the end point unifies with
|ToIndex|. 

The hook |delete_connection_hook/2| is evaluated with the instantiated
|FromIndex| and |ToIndex| as argument for every connection being removed.

The following example deletes all connections stored in the matrix.
\begin{BoxedSample}
  ?- delete\_connection(\_,\_),fail ; true.
\end{BoxedSample}

\PL*/	
delete_connection(From,To) :-
	'Connection'(From,To),
	run_hooks(delete_connection,[From,To]),
	make_c_to_key(From,KeyAB),
	erase(KeyAB,To),
	make_c_from_key(To,KeyCD),
	erase(KeyCD,From).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_literal/1(+Index).

This predicate deletes the literal identified by |Index| from the matrix. Any
information is abolished. All connections to and from this literal are
deleted. The literal does not occur any longer in any contrapositive or the
whole clause.

The literal has to be sufficiently instantiated. The predicate fails if the
specified literal does not exist.

The hook |delete_literal_hook/1| is evaluated before the literal is removed.
The argument of this hook is the index of the literal to be deleted.

The hooks |delete_contrapositive_hook/1| and |delete_connection_hook/2| may be
called when the respective information is deleted.

The following example deletes the literal |3-2| from the matrix.
\begin{BoxedSample}
  ?- delete\_literal(3-2).
\end{BoxedSample}

\PL*/
delete_literal(Index) :-
	recorded('#index',Index),
	run_hooks(delete_literal_hook,[Index]),
	(   delete_connection(Index,_),
	    fail
	;   delete_connection(_,Index),
	    fail
	;   true
	),
	(delete_contrapositive(Index) -> true ; true ),
	Index = CI-LI,
	(   recorded('#index',CI-I),
	    I \== LI,
	    make_index_key(CI,I,IKey),
	    erase(IKey,contrapositive(Literal,Body,K)),
	    Literal =.. [Sign,Pred],
	    functor(Pred,F,A),
	    make_pred_key(Sign,F,A,Key),
	    erase(IKey,contrapositive(Literal,Body,K)),
	    delete(literal(_,Index),Body,NewBody),
	    record(Key,contrapositive(Literal,NewBody,K)),
	    record(IKey,contrapositive(Literal,NewBody,K))
	;   true
	),
	( erase('#Clause',CI=Body2) ->
	    delete(literal(_,Index),Body2,NewBody2),
	    record('#Clause',CI=NewBody2)
	;   true
	),
	analyze_lits(NewBody,Pos,Neg),
	( Neg = 0 ->
	    ( recorded('#positive_clause',CI) ->
		true
	    ;   record('#positive_clause',CI) 
	    )
	; recorded('#positive_clause',CI) ->
	    erase('#positive_clause',CI)
	;   true
	),
	( Pos = 0 ->
	    ( recorded('#negative_clause',CI) ->
		true
	    ;   record('#negative_clause',CI) 
	    )
	; recorded('#negative_clause',CI) ->
	    erase('#negative_clause',CI)
	;   true
	),
	make_length_key(CI,KeyL),
	erase(KeyL,Length),
	NewLength is Length - 1,
	record(KeyL,NewLength),
	erase('#index',Index).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate analyze_lits/3(+LiteralList, ?Positive, ?Negative).

This predicate counts the positive |Positive| and the negative literals
|Negative| in a list of literals |LiteralList|.

\PL*/
analyze_lits([],0,0).
analyze_lits([literal(L,_)|T],Pos1,Neg1) :-
	analyze_lits(T,Pos,Neg),
	( functor(L,(++),2) ->
	    Pos1 is Pos+1,
	    Neg1 = Neg
	;   Pos1 = Pos,
	    Neg1 is Neg+1
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_goal_label/1(?ClauseIndex).

This predicate deletes the goal label of the given clause. |ClauseIndex| is in
turn unified with all clause indices having a goal label. The   predicate is
resatisfiable until no more unifying goal clause is present.
\begin{BoxedSample}
  ?- delete\_goal\_label(CI).
  CI = 4
  ;
  No more solution
\end{BoxedSample}

\PL*/
delete_goal_label(ClauseIndex) :-
	erase('#goal',ClauseIndex).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate delete_label/2(?ClauseIndex, ?Label).

This predicate deletes the label |Label| of the given clause |ClauseIndex|.
The predicate is resatisfiable until no more labels are present.
\begin{BoxedSample}
  ?- delete\_label(CI,Label).
  CI = 2
  Label = base
  ;
  CI = 3
  Label = loop
  ;
  No more solution
\end{BoxedSample}

\PL*/
delete_label(ClauseIndex,Label) :-
	erase('#label',ClauseIndex=Label).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_matrix/2(+Prefix, +Stream).

The current matrix is printed onto the stream |Stream|.  Each line written is
preceded by the string |Prefix|.  The format used is suitable for reading it
back in if |Prefix| is the empty string.

|Stream| can also be a symbolic stream of \eclipse. E.g. |output| directs the
matrix to the standard output stream.
\begin{BoxedSample}
  ?- show\_matrix("\%", output).
  \%      [ \% Clause 1
  \%        ++ p(X),
  \%        -- q(f(X),g(X))
  \%      ].
  \% base:: [  \% Clause 2
  \%        ++ q(a,X)
  \%      ].
  \% loop:: [  \% Clause 3
  \%        ++ q(X,Y),
  \%        -- q(f(X),Y)
  \%      ].
  \% ?-   [  \% Clause 4
  \%         -- p(Z)
  \%      ].
\end{BoxedSample}

\PL*/
show_matrix(Prefix,Stream) :-
	(   'Clause'(CI,Body),
	    ( 'GoalClause'(CI) ->
		printf(Stream,"%w ?-",[Prefix])
	    ;   printf(Stream,"%w",[Prefix])
	    ),
	    (   'Label'(CI,Label),
		printf(Stream,"%QDMw::\n%w",[Label,Prefix]),
		fail
	    ;   true
	    ),
	    ( Body = [literal(L1,_)|Rest] ->
		printf(Stream,"\t[ %% Clause %w\n",[CI]),
		printf(Stream,"%w\t  %QDMw",[Prefix,L1]),
		(   member(literal(L,_),Rest),
		    printf(Stream,",\n%w\t  %QDMw",[Prefix,L]),
		    fail
		;   true
		),
		printf(Stream,"\n%w\t].\n",[Prefix])
	    ;   printf(Stream,"%w\t[]. %% Clause %w\n",[Prefix,CI])
	    ),
	    fail
	;   true
	).	    
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate show_contrapositives/2(+Prefix, +Stream).

The current list of contrapositives is printed onto the stream |Stream|.  Each
line written is preceded by the string |Prefix|.

|Stream| can also be a symbolic stream of \eclipse. E.g. |output| directs the
matrix to the standard output stream.
\begin{BoxedSample}
  ?- show\_contrapositives(" ", output).
    ++ p(X) <- \% (1 - 1)
           -- q(f(X),g(X)).
    -- q(f(X),g(X)) <- \% (1 - 2)
           ++ p(X).
    ++ q(a,X). \% (2 - 1)
    ++ q(X,Y) <- \% (3 - 1)
           -- q(f(X),Y).
    -- q(f(X),Y) <- \% (3 - 2)
           ++ q(X,Y).
    -- p(Z). \% (4 - 1)
\end{BoxedSample}

\PL*/
show_contrapositives(Prefix,Stream) :-
	(   'Contrapositive'(Head,Body,CI),
	    ( Body = [literal(L1,_)|Rest] ->
		printf(Stream,"%w%QDMw <- %% (%w)\n",
			      [Prefix,Head,CI]),
		printf(Stream,"%w\t%QDMw",[Prefix,L1]),
		(   member(literal(L,_),Rest),
		    printf(Stream,",\n%w\t%QDMw",[Prefix,L]),
		    fail
		;   true
		),
		printf(Stream,".\n",[])
	    ;   printf(Stream,"%w%QDMw. %% (%w)\n",
			      [Prefix,Head,CI])
	    ),
	    fail
	;   true
	).	    
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\EndProlog*/
