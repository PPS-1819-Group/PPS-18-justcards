startHand(3).
pointsToWinSession(3).
winnerPoints(VAR,1).
drawPoints(VAR,0).
chooseBriscola(0).
loserPoints(VAR,0).
draw(1).
card(2,VAR,1,0) :- 	seed(VAR).
card(4,VAR,2,0) :- 	seed(VAR).
card(5,VAR,3,0) :- 	seed(VAR).
card(6,VAR,4,0) :- 	seed(VAR).
card(7,VAR,5,0) :- 	seed(VAR).
card(8,VAR,6,2) :- 	seed(VAR).
card(9,VAR,7,3) :- 	seed(VAR).
card(10,VAR,8,4) :- 	seed(VAR).
card(3,VAR,9,10) :- 	seed(VAR).
card(1,VAR,10,11) :- 	seed(VAR).
points(VAR,VAR).
