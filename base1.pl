:- use_module(library(clpfd)).

animal(chien).
animal(chat).
prenom(paul).
prenom(pierre).
prenom(jean).
possede(jean,chat).
possede(pierre,chien).
possede(pierre,cheval).
amis(pierre,jean).
amis(jean,pierre).
amis(jean,paul).
amis(pierre,paul).
amis(paul,jacques).
amis_2(X,Z):-amis(X,Y),amis(Y,Z),X\=Z.
amis(X):-amis(X,_).

homme(hugo).
homme(loic).
homme(gabriel).
homme(maxime).
homme(mathieu).
homme(alexis).
femme(catherine).
femme(justine).
femme(lea).
femme(alice).
femme(rose).
femme(emma).
parent(hugo,lea).
parent(hugo,gabriel).
parent(catherine,lea).
parent(catherine,gabriel).
parent(loic,alice).
parent(loic,maxime).
parent(loic,mathieu).
parent(justine,alice).
parent(justine,maxime).
parent(justine,mathieu).
parent(gabriel,alexis).
parent(gabriel,rose).
parent(gabriel,emma).
parent(alice,alexis).
parent(alice,rose).
parent(alice,emma).

enfant(X,Y) :- parent(Y,X).
fille(X,Y) :- parent(Y,X) , femme(X).
fils(X,Y) :- parent(Y,X) , homme(X).
mere(X,Y) :- parent(X,Y) , femme(X).
pere(X,Y) :- parent(X,Y) , homme(X).
oncle(X,Y) :- fils(X,Z) , parent(Z,A) , parent(A,Y), homme(Z),A\=X.
tante(X,Y) :- fille(X,Z) , parent(Z,A) , parent(A,Y), femme(Z), A\=X.
grand_parent(X,Y) :- parent(Z,Y), parent(X,Z).
grand_pere(X,Y) :- grand_parent(X,Y), homme(X).
grand_mere(X,Y) :- grand_parent(X,Y), femme(X).
petit_enfants(X,Y) :- grand_parent(Y,X).
petite_fille(X,Y) :- petit_enfants(X,Y), femme(X).
petit_fils(X,Y) :- petit_enfants(X,Y), homme(X).
neveu(X,Y) :- (oncle(Y,X) ; tante(Y,X)) , homme(X).
niece(X,Y) :- (oncle(Y,X) ; tante(Y,X)) , femme(X).
frere(X,Y) :- pere(Z,X) , pere(Z,Y), homme(X), X\=Y.
soeur(X,Y) :- mere(Z,X) , mere(Z,Y), femme(X), X\=Y.

% 4.3

sum(X,Y,R):- R is X+Y.
max2(X,Y,M) :- X>Y, M = X.
max2(_,Y,M) :- M = Y.
max2(X,Y,Z,M) :- max2(X,Y,T) , max2(T,Z,M).

d(X,X,1).
d(C,X,0) :- atomic(C), C\=X.
d(A+B,X,C+D):- d(A,X,C), d(B,X,D).
d(A-B,X,C-D):- d(A,X,C), d(B,X,D).
d(A*B,X,C*B + A*D) :- d(A,X,C), d(B,X,D).
d(X^C,X,C*X^(C-1)).



word(abandon, a, b, a, n, d, o, n).
word(abalone, a, b, a, l ,o ,n,e ).
word(enhance, e, n, h, a, n, c, e).
word(anagram, a, n, a, g, r, a, m).
word(connect, c, o, n, n, e, c, t).
word(elegant, e, l, e, g, a, n, t).
crossword(V1,V2,V3,H1,H2,H3) :- word(V1, _, V1B, _, V1D, _, V1F, _),
								word(V2, _, V2B, _, V2D, _, V2F, _),
								word(V3, _, V3B, _, V3D, _, V3F, _),
								word(H1, _, V1B, _, V2B, _, V3B, _),
								word(H2, _, V1D, _, V2D, _, V3D, _),
								word(H3, _, V1F, _, V2F, _, V3F, _).

longeur([],0).
longeur([_|T], N) :- longeur(T,N1) , N is N1+1.

max([X],X).
max([X|T],M) :-  max(T,M1),  max2(X,M1,M).

mymax([Max], Max).
mymax([Head | List], Max) :-
  mymax(List, MaxList),
  ( Head > MaxList -> Max = Head ; Max = MaxList ).

somme([],0).
	somme([X|Y],S) :- somme(Y,S1) , S is X+S1.

  nth(1,[X|_],X):- !.
	nth(N,[_|T],R) :- N>0, N1 is N-1 , nth(N1,T,R) .

append([],L,L).
append([X|T],L2,[X|L3]) :- append(T,L2,L3).

zip([],[],[]).
zip([A|C],[B|D],[[A,B]|X]):-zip(C,D,X).

enumerate(1,[0]).
enumerate(N,L):- N>=1, N1 is N-1,enumerate(N1,L2),append(L2,[N1],L).

calcul_monnaie(Argent, amount(TDo,Do,Qu,Ten,Fi)):-
TDo is Argent div 200,
Remain1 is Argent mod 200,
Do is Remain1 div 100,
Remain2 is Remain1 mod 100,
Qu is Remain2 div 25,
Remain3 is Remain2 mod 25,
Ten is Remain3 div 10,
Remain4 is Remain3 mod 10,
Fi is Remain4 div 5.

rend_monnaie(Argent,Prix):- X is floor(100*(Argent-Prix)+0.001),calcul_monnaie(X,amount(TDo,Do,Qu,Ten,Fi)), write('A rendre :'), nl, write(Fi), write(' piece de 0.05'), nl, write(Ten), write(' piece de 0.10'), nl,
write(Qu), write(' piece de 0.25'), nl,
write(Do), write(' piece de 1'), nl,
write(TDo), write(' piece de 2'), nl.


house_1(Vars):-
	Vars=[Rouge, Bleu, Anglais, Espagnol, Japonais, Jaguar, Escargot, Serpent],
	Vars ins 1..3,
	Rouge #= Anglais,
	Espagnol #= Jaguar,
	Japonais #= Escargot-1,
	Bleu #= Escargot+1,
	
	all_distinct([Rouge,Bleu]),
	all_distinct([Jaguar,Escargot,Serpent]),
	all_distinct([Anglais,Japonais,Espagnol]).


house_final(Vars):-
	Vars = [Rouge, Vert, Blanc, Jaune, Bleu, Anglais, Espagnol, Ukrainien, Norvegien, Japonais,
	Chien, Escargot, Renard, Cheval, Zebre, Cafe, The, Lait, Jus, Eau, Sculpteur, Diplomate, Violoniste, Medecin, Acrobate],
	Vars ins 1..5,
	Anglais #= Rouge,
	Espagnol #= Chien,
	Vert#=Cafe,
	Ukrainien#=The,
	Vert#=Blanc+1,
	Sculpteur#=Escargot,
	Diplomate#=Jaune,
	Lait#=3,
	Norvegien#=1,
	Medecin#=Renard-1#\/Medecin#=Renard+1,
	Diplomate#=Cheval-1#\/Diplomate#=Cheval+1,
	Violoniste#=Jus,
	Japonais#=Acrobate,
	Norvegien#=Bleu-1,
	all_distinct([Rouge, Vert, Blanc, Jaune, Bleu]),
	all_distinct([Anglais, Espagnol, Ukrainien, Norvegien, Japonais]),
	all_distinct([Chien, Escargot, Renard, Cheval, Zebre]),
	all_distinct([Cafe, The, Lait, Jus, Eau]),
	all_distinct([Sculpteur, Diplomate, Violoniste, Medecin, Acrobate]),
	label(Vars).

house_2(Vars):-house_final(Vars), printer(Vars).


printer([]).
printer([H|T]):- write(H),write(','), printer(T).

serpent_proprietaire(Prop):- maison(Hs),member(h(Prop,serpent,_), Hs).

maison(Hs):-
	length(Hs,3),
	member(h(anglais,_,rouge),Hs),
	member(h(espagnol,jaguar,_),Hs),
	droite(h(japonais,_,_) , h(_,escargot,_), Hs),
	gauche(h(_,escargot,_) , h(_,_,bleu), Hs),
	member(h(_,serpent,_), Hs).

droite(X,Y,Ls):- append(_,[Y,X|_],Ls).
gauche(X,Y,Ls):- append(_,[X,Y|_],Ls).
