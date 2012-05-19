#!/usr/bin/pl -q -t main -f
:-module(tparser,[ex1/0,ex1/0,ex2/0,ex3/0,ex4/0,ex5/0,
	if_ex/0,while_ex/0,fl1/0,fl2/0,pex1/0,pex2/0,pex3/0,
	pex4/0,fex1/0,fex2/0,compile/0,c2/0,create_table/0,ct/0]).

/*Grammer*/
/*non_terminal*/
n(e).
n(ep).
n(t).
n(tp).
n(f).
n(d).
n(dp).
n(type).
n(p).
/*terminal*/
t('(').
t(')').
t('*').
t('+').
t('-').
t('/').
t('int').
t('bool').
t(';').
t(':').
t('if').
t('then').
t('else').
t('while').
t('do').
t(':=').
t(Id).


/*rule(Non-(terminal,rhs)*......n()-repesent non terminal and t() represt terminal */
/*left-recursion removed grammer...is also already left-factored*/
/* as A->LamdaBeta1 ,A->LamdaBeta2 does not exists*/
rule(n(p),[n(d),t(';'),n(s)]).
rule(n(d),[t(Id),t(':'),n(type),n(dp)]).
rule(n(dp),[t(';'),n(d),n(dp)]).
rule(n(dp),[]).

rule(n(s),[n(e),n(sp)]).
rule(n(s),[t('if'),n(e),t('then'),n(s),n(sp)]).
rule(n(s),[t('while'),n(e),t('do'),n(s),n(sp)]).
rule(n(sp),[t(';'),n(s),n(sp)]).
rule(n(sp),[]).
rule(n(s),[]).

rule(n(e),[n(t),n(ep)]).
rule(n(ep),[t('+'),n(t),n(ep)]).
rule(n(ep),[t('-'),n(t),n(ep)]).

rule(n(ep),[]).

rule(n(t),[n(f),n(tp)]).
rule(n(tp), [t('*'), n(f),n(tp)]).
rule(n(tp), [t('/'),n(f),n(tp)]).
rule(n(tp), [t(':='), n(f),n(tp)]).
rule(n(tp),[]).
rule(n(type),[t(int)]).
rule(n(type),[t(bool)]).
rule(n(f),[t('('),n(e),t(')')]).
rule(n(f),[t(Id)]).


ex0:-
	parse([t(key),t(':'),t('int'),t(';'),t('('),t(5),t(')'),$],[n(p)]).
ex1:-
	parse([t(key),t(':'),t('int'),t(';'),t(1),t('+'),t(2),t(';'),t(5),t('+'),t(5),t(';'),$],[n(p)]).
ex2:-
        parse([t('('),t(1),t('+'),t(2),t(')'),$],[n(e)]).
ex3:-
	parse([t('('),t(1),t('+'),t(2),t(')'),t('*'),t(4),$],[n(e)]).
/* subtractioin and division example */
ex4:-
	parse([t(1),t('-'),t(2),$],[n(e)]).
ex5:-
	parse([t('('),t(1),t('+'),t(2),t(')'),t('/'),t(4),$],[n(e)]).
if_ex:-
	parse([t(key),t(':'),t('int'),t(';'),t('if'),t('('),t(5),t(':='),t(5),t(')'),t('then'),t(1),t('+'),t(2),t(';'),$],[n(p)]).
while_ex:-
	parse([t(key),t(':'),t('int'),t(';'),t('while'),t('('),t(5),t(':='),t(5),t(')'),t('do'),t(1),t('+'),t(2),t(';'),$],[n(p)]).	
/* fails example with starting ')'.*/
fl1:-
	parse([t(')'),t(1),t('+'),t(2),t(')'),t('*'),t(4),$],[n(e)]).
/*no multipolier or addition after bracket close*/

fl2:-
	parse([t(')'),t(1),t('+'),t(2),t(')'),t(4),$],[n(e)]).

parse([$],[]).


parse(Input,[n(N)|Stack]):-
	rule(n(N),Rhs),
	append(Rhs, Stack, NewStack),
	parse(Input, NewStack).
parse([t(W)|Input], [t(W)| Stack]) :- 
	parse(Input,Stack).



/*Hard coded Parse table*/

/*Hard coded Parse table*/

/*terminal + */
/*terminal Id has to be at the end otherwise other terminal will bind to it first*/
mtable(n(e),t('+'),[error],[]).
mtable(n(ep),t('+'),[t('+'),n(t),n(ep)],mknode('+')).  /*mknode here*/
mtable(n(t),t('+'),[error],[]).
mtable(n(tp),t('+'),[],[]).
mtable(n(f),t('+'),[error],[]).
/*terminal '-' */
mtable(n(e),t('-'),[error],[]).
mtable(n(ep),t('-'),[t('-'),n(t),n(ep)],mknode('-')). /*mknode here*/
mtable(n(t),t('-'),[error],[]).
mtable(n(tp),t('-'),[],[]).
mtable(n(f),t('-'),[error],[]).
/* terminal '*' */
mtable(n(e),t('*'),[error],[]).
mtable(n(ep),t('*'),[error],[]).
mtable(n(t),t('*'),[error],[]).
mtable(n(tp),t('*'),[t('*'), n(f), n(tp)],mknode('*')).
mtable(n(f),t('*'),[error],[]).
/*terminal '/' */
mtable(n(e),t('/'),[error],[]).
mtable(n(ep),t('/'),[error],[]).
mtable(n(t),t('/'),[error],[]).
mtable(n(tp),t('/'),[t('/'), n(f), n(tp)],mknode('/')).
mtable(n(f),t('/'),[error],[]).
/*terminal '(')  */
mtable(n(e),t('('),[n(t),n(ep)],[]).
mtable(n(ep),t('('),[error],[]).
mtable(n(t),t('('),[n(f),n(tp)],[]).
mtable(n(tp),t('('),[error],[]).
mtable(n(f),t('('),[t('('),n(e),t(')')],[]).
/* terminal ')' */
mtable(n(e),t(')'),[error],[]).
mtable(n(ep),t(')'),[],[]).
mtable(n(t),t(')'),[error],[]).
mtable(n(tp),t(')'),[],[]).
mtable(n(f),t(')'),[error],[]).
/* terminal $ */
mtable(n(e),t('$'),[error],[]).
mtable(n(ep),t('$'),[],[]).
mtable(n(t),t('$'),[error],[]).
mtable(n(tp),t('$'),[],[]).
mtable(n(f),t('$'),[error],[]).
/* terminal Id */
mtable(n(e),t(Id),[n(t),n(ep)],[]).
mtable(n(ep),t(Id),[error],[]).
mtable(n(t),t(Id),[n(f),n(tp)],[]).
mtable(n(tp),t(Id),[error],[]).
mtable(n(f),t(Id),[t(Id)],mkleaf(id,Id)).
	
	    /* mkleaf here */
/*predictive parser with two dimensional parsing table M*/
/*first=Input, second para is stack=[n(e),$], */
/* p_parse([n(e),$],[]). */

/*simple pass example */
pex1:-
	parse_with_table([t(5),t(+),t(6),t('$')],[n(e),t('$')],[]).
pex2:-
	parse_with_table([t(5),t(+),t(6),t(-),t(7),t(+),t(3),t('$')],[n(e),t('$')],[]).	
fex2:-
	parse_with_table([t(+),t(6),t(5),t('$')],[n(e),t('$')],[]).	
/*pass example with subtraction and division */
pex3:-
	parse_with_table([t('('),t(5),t(-),t(6),t(')'),t('/'),t(7),t('$')],[n(e),t('$')],[]).

/*fail example */	
fex1:-
	parse_with_table([t(5),t(')'),t(6),t('$')],[n(e),t('$')]).
	
pex4:-
	parse_with_table([t(5),t('/'),t(6),t('$')],[n(e),t('$')],[]).
	
/*our tree structure */
t(Root,Leaf).
t([t(Root,Leaf)],Root,Leaf).
t([t(Root,[L1,R1])],Root,L1,R1).
t([],[],[]).


/*make nodes */
mkleaf(id,Id,t(id,Id)).
mkleaf(num,Num,t(num,Num)).

mkNode(Root,Nodes,[t(Root,Nodes)]).
	
/*mkNode(Node).*/

stich(Root,Tres,[t(Root,Tres)]).

createtree(tree,Tree,Tres).
/* empty the leaves */

createtree(mkleaf(id,Id),Tree,Tres):-
		/*arg(2,t(_,Tree),Y),*/
		/*so add to node if there*/
		t(Tree,Root,Leaves),
		append(Leaves,[t(id,Id)],Ts),
		 (  Root =[]
		  -> Tres=Ts;
		     Tres=[t(Root,Ts)]  
		 ).
		
createtree(mknode(Root),Leaves,[t(Root,Leaves)]).						

createtree([],Tree,Tree).


ops(t('-'),t(id,IdL),t(id,IdR),Ev):-
			Ev is IdL - IdR.
			
ops(t('*'),t(id,IdL),t(id,IdR),Ev):-
			Ev is IdL * IdR.
			
ops(t('/'),t(id,IdL),t(id,IdR),Ev):-
			Ev is IdL / IdR.
			
ops(t('+'),t(id,IdL),t(id,IdR),Ev):-
			Ev is IdL + IdR.			
				


calcfromstack([((Root),L1,t(id,IdR))|T],Evaluated):-
		(L1=t(id,IdL)
		  ->ops(t(Root),t(id,IdL),t(id,IdR),Res);
		    ops(t(Root),Evaluated,t(id,IdR),Res)
	        ),
		io:format("Res ~p~n",[Res]),
		calcfromstack(T,t(id,Res)).
		
calcfromstack([],t(id,Final)):-
			io:format("final value is ~p~n",[Final]).
		
/* how the last factor will end so cannot have [] but this*/	
secondPass(Root,Stack,[t(id,IdL)],[t(id,IdR)],Result):-
			calcfromstack(Stack,Evaluated).
			
secondPass(Root,Stack,Leaf,R,Result):-
		t(Leaf,Roo,L1,R1),
		io:format("evaluating root: ~p~n",[Roo]),
		io:format("evaluating left leaf: ~p~n",[L1]),
		io:format("evaluating right leaf: ~p~n",[R1]),
	   
		/*eval(Root,L1,R1,Result).*/
		/* as this is left associative we pass left leaf */
		secondPass(Roo,[(Roo,L1,R1)|Stack],[L1],[R1],Result).
compile:-
		/*for parse_with_table([t(5),t(+),t(6),t(-),t(7),t(+),t(3),t('$')],[n(e),t('$')],[]).	*/
		Tree=[t(+,[t(-,[t(+,[t(id,5),t(id,6)]),t(id,7)]),t(id,3)])],
		secondPass(Root,[],Tree,R,Result).	

c2:-
	t([t(+,[t(id,5),t(id,6)])],Root,L1,R1),
	t(L1).
		
/* final stack result for succesful parse  */	
parse_with_table([t('$')],[t('$')],Tree):-
	io:format("~p~n",[Tree]).
	
	/* second pass of  parser ...evaluation on tree */
	/*secondPass(Tree)*/
	
/*initiall stack should have [n(e),$] */
parse_with_table([H|T],[n(N)|Stack],Tree):-
	mtable(n(N),H,Rhs,Action),
	createtree(Action,Tree,Tres),
	append(Rhs, Stack, NewStack),
	parse_with_table([H|T], NewStack,Tres).		
/* second rule of parser to pop X off the stack and advance the input pointer to the next input symbol */

parse_with_table([t(W)|T], [t(W)| Stack],Tree) :- 
	parse_with_table(T,Stack,Tree).	
parse_with_table(I,[error|T],Tree):-
			fail.
first([first(n(e),[t('('),t('Id')]),
     first(n(t),[t('('),t('Id')]),
     first(n(f),[t('('),t('Id')]),
     first(n(ep),[t('+'),t('-'),t('[]')]),
     first(n(tp),[t('*'),t('/'),t('[]')])]).
     
follow([follow(n(e),[t(')'),t('$')]),
     follow(n(ep),[t(')'),t('$')]),
     follow(n(t),[t('+'),t('-'),t(')'),t('$')]),
     follow(n(tp),[t('+'),t('-'),t(')'),t('$')]),
     follow(n(f),[t('+'),t('*'),t('/'),t(')'),t('$')])]).
     
/*helper term should be auto generated */
first_w_empty([first(n(ep),[t('+'),t('-'),t('[]')]),
     		first(n(tp),[t('*'),t('/'),t('[]')])]).
follow_w_emty_in_first([follow(n(ep),[t(')'),t('$')]),
			follow(n(tp),[t('+'),t('-'),t(')'),t('$')])]).


/*list of terminals in the first */
match_and_write(Non_terminal,[H|T]):-
	rule(Non_terminal,Rhs),	
	Term=mt(Non_terminal,H,Rhs),
        write(Term),write('.'),nl,
        match_and_write(Non_terminal,T).
match_and_write(_,[]):-
	  true.
			

		
/*the list here gets all the first methods */			
f_and_f2([H|T]):-
		 H=first(Non_terminal,List_of_terminal),
		 /*remove empty but later do something in f&f rule 3 */
		 delete(List_of_terminal,t('[]'),L1),
		 match_and_write(Non_terminal,L1),
		 f_and_f2(T).
		 
f_and_f2([]).


follow_with_empty_in_first([],S):- true.

follow_with_empty_in_first([H|T],S):-
		 H=first(_,List_of_terminal),
		(member(t('[]'),List_of_terminal)->
			follow_with_empty_in_first(T,[H|S]);
			follow_with_empty_in_first(T,S)).
			
		
/*will need optimzed crude no need to go through all use match */
/*implement with generic extract b*/
		
forfollow(Nt,[H|T]):-
	H=follow(Non_terminal,List_of_terminal),
	(Nt==Non_terminal->
			 match_and_write(Non_terminal,List_of_terminal),
			 forfollow(Nt,T);
			 forfollow(Nt,T)).
			 
forfollow(_,[]).


		
f_and_f3([H|T]):-
		H=first(Nt,List_of_terminal),
		(member(t('[]'),List_of_terminal)->
			follow(L),
			forfollow(Nt,L),
			f_and_f3(T);
			f_and_f3(T)).
		
f_and_f3([]).		
		
		


		
f_and_f3([]).
		
table_creation :-
		first(L),
		 /*rule 2 */
		f_and_f2(L),
		/*rule 3 */
		/*need to be inplementes*/
	        follow_with_empty_in_first(L,S),
		f_and_f3(L).
		

/* parse table load*/			
load_table:-
	consult(table).		

/*parse tree creation after change in grammer */
			
create_table:-
	tell('table.pl'),
	table_creation,
	told.

ct:-
	create_table.			
			


/*for recognizing S->aSb,S->c*/
/* simple recursive descent parser */
sent2(ASB):-
		append([a],SB,ASB),
		append(S,[b],SB),
		sent2(S).
		
sent2([c]).

	
not( P) :-
	P, !, fail;
	true.
	
sim(X,Y,X):-X<Y.
sim(X,Y,Y):-Y<X.

ss:-
	sim(7,4,X),
	sim(4,7,X),
	
	X.
max(X,Y,Z) :-
    (  X =< Y
    -> Z = Y
    ;  Z = X  
     ).

	

getwd(Wd) :-
        seeing(Old), see(pipe(pwd)), 
        collect_wd(String), 
        seen, see(Old), 
        atom_chars(Wd, String).

collect_wd([C|R]) :-
        get0(C), C \== -1, !, 
        collect_wd(R).
collect_wd([]).

eval :-
        current_prolog_flag(argv, Argv),
        append(_, [--|Args], Argv),
        concat_atom(Args, ' ', SingleArg),
        term_to_atom(Term, SingleArg),
        Val is Term,
        format('~w~n', [Val]).

main :-
        catch(eval, E, (print_message(error, E), fail)),
        halt.
main :-
        halt(1).
