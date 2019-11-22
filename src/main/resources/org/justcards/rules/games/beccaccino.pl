% startHand(+CardsInHand)
startHand(10).

% draw(+CardsToDraw)
draw(0).

% Knowledge to know if is mandatory to play the same seed of the first card in the field
playSameSeed.

/* chooseBriscola(+Choice)
	1 => the user will choose the briscola
	0 => the system will choose the briscola
	-1 => the game does not have a briscola
*/
chooseBriscola(1).

% currentBriscola(+Briscola).
% Has to be changed at runtime every match if chooseBriscola(1|0)
currentBriscola(denara).

pointsToWinSession(41).
winnerPoints(X,Y):- Y is X/3.
loserPoints(X,Y):- Y is X/3.
drawPoints(X,0).

% Knowledge to know if the last take of a match has one more point
lastTakeHasOneMorePoint.

% card(+Number,+Seed,+Hierarchy,+Value)
card(1,Seed,8,3):- seed(Seed).
card(2,Seed,9,1):- seed(Seed).
card(3,Seed,10,1):- seed(Seed).
card(4,Seed,1,0):- seed(Seed).
card(5,Seed,2,0):- seed(Seed).
card(6,Seed,3,0):- seed(Seed).
card(7,Seed,4,0):- seed(Seed).
card(8,Seed,5,1):- seed(Seed).
card(9,Seed,6,1):- seed(Seed).
card(10,Seed,7,1):- seed(Seed).