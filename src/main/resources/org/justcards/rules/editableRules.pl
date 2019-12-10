numberInRange(X,Max,Min):-
	number(X),
	X < Max + 1,
  X > Min - 1,
  !.

gamePlayers(4).

count([],0).
count([H|Tail], N) :-
    count(Tail, N1),
    N is N1 + 1.

deckCards(Cards,CardsCount):-
	findall(card(Number,Seed),card(Number,Seed),Cards),
	count(Cards,CardsCount).

cardsDistribution(UserStartHand,UserDrawPerTurn,UserStartField):-
	draw(UserDrawPerTurn),
	startHand(UserStartHand),
	startField(UserStartField),
	deckCards(_,CardsCount),
	gamePlayers(GamePlayersCount),
	UsersStartHand is UserStartHand * GamePlayersCount,
	CardsWithoutUsersStartHand is CardsCount - UsersStartHand,
	RemainingCards is CardsWithoutUsersStartHand - UserStartField,
	UsersDrawPerTurn is UserDrawPerTurn * GamePlayersCount,
	UsersDrawPerTurn < RemainingCards + 1,
	0 is RemainingCards mod UsersDrawPerTurn.

draw(UserDrawPerTurn):-
	number(UserDrawPerTurn),
	minDraw(Min),
	UserDrawPerTurn > Min - 1.

minDraw(0).

startHand(UserStartHand):-
	number(UserStartHand),
  minStartHand(Min),
  UserStartHand > Min - 1.

minStartHand(0).

startField(0).

pointsToWinSession(X):-
	number(X),
	minPointsToWinSession(Min),
	X > Min - 1.
minPointsToWinSession(1).

points(X):-
	integer(X),
	X > -1.

starterCard(X,Y):- card(X,Y).

seed(bastoni).
seed(denara).
seed(spade).
seed(coppe).

% card(+Number,+Seed)
card(1,Seed):- seed(Seed).
card(2,Seed):- seed(Seed).
card(3,Seed):- seed(Seed).
card(4,Seed):- seed(Seed).
card(5,Seed):- seed(Seed).
card(6,Seed):- seed(Seed).
card(7,Seed):- seed(Seed).
card(8,Seed):- seed(Seed).
card(9,Seed):- seed(Seed).
card(10,Seed):- seed(Seed).