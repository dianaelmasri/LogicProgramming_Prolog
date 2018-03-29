
pere(hugo,gabriel).
pere(gabriel,alexis).
pere(alexis,paul).
pere(paul,andre).
ancetre(X,Y):-pere(X,Y).
ancetre(X,Y):-pere(X,Z),ancetre(Z,Y).