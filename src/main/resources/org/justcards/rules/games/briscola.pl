startHand(3).
pointsToWinSession(3).
winnerPoints(VAR,1).
drawPoints(VAR,0).
chooseBriscola(0).
loserPoints(VAR,0).
draw(1).
card(1,VAR,1,11) :- 	seed(VAR).
card(3,VAR,2,10) :- 	seed(VAR).
card(10,VAR,3,4) :- 	seed(VAR).
card(9,VAR,4,3) :- 	seed(VAR).
card(8,VAR,5,2) :- 	seed(VAR).
card(7,VAR,6,0) :- 	seed(VAR).
card(6,VAR,7,0) :- 	seed(VAR).
card(5,VAR,8,0) :- 	seed(VAR).
card(4,VAR,9,0) :- 	seed(VAR).
card(2,VAR,10,0) :- 	seed(VAR).
points(VAR,VAR).
