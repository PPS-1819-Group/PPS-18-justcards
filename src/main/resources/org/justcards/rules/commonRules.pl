% Game seeds
seed(bastoni).
seed(coppe).
seed(spade).
seed(denara).

% card(+Number,+Seed)
card(Number,Seed):-
	seed(Seed),
	card(Number,Seed,_,_).

% briscola(+Briscola)
briscola(Briscola):- seed(Briscola).

% hand(+(Number,Seed),+Cards)
hand((Number,Seed),Cards):-
	card(Number,Seed),
	member((Number,Seed),Cards),
	!.

% starterPlayer(+Cards)
starterPlayer([(Number,Seed)|T]):- starterCard(Number,Seed), !.
starterPlayer([(Number,Seed)|T]):- starterPlayer(T).

% sessionStarterPlayer(+PlayersCards,-FirstSessionPlayer)
sessionStarterPlayer([(Player,Cards)|T],Player):- starterPlayer(Cards), !.
sessionStarterPlayer([(Player,Cards)|T],FirstSessionPlayer):- sessionStarterPlayer(T,FirstSessionPlayer).

% turn(+(Number,Seed),+Field,+Hand,-NewField)
turn((Number,Seed),[],Hand,[(Number,Seed)]):- hand((Number,Seed),Hand), !.

turn((MyNumber,MySeed),[(FieldNumber,MySeed)|FieldCards],Hand,NewField):- 
	hand((MyNumber,MySeed),Hand),
	append([(FieldNumber,MySeed)|FieldCards],[(MyNumber,MySeed)],NewField), 
	!.

turn((MyNumber,MySeed),[(FieldNumber,FieldSeed)|FieldCards],Hand,NewField):-
	MySeed \= FieldSeed,
	playSameSeed,
	hand((MyNumber,MySeed),Hand),
	not(hand((_,FieldSeed),Hand)),
	append([(FieldNumber,FieldSeed)|FieldCards],[(MyNumber,MySeed)],NewField),
	!.

turn((MyNumber,MySeed),[(FieldNumber,FieldSeed)|FieldCards],Hand,NewField):-
	MySeed \= FieldSeed,
	not(playSameSeed),
	hand((MyNumber,MySeed),Hand),
	append([(FieldNumber,FieldSeed)|FieldCards],[(MyNumber,MySeed)],NewField),
	!.

% fieldWinner(+FieldCards,-Winner)
fieldWinner([(Number,Seed,Player)|T],Winner):- fieldWinner(T,Winner,(Number,Seed,Player)).

fieldWinner([],Winner,(_,_,Winner)).

% same seed
fieldWinner([(Number,Seed,Player)|T],Winner,(NumberWinner,Seed,PlayerWinner)):-
	card(Number,Seed,Hierarchy,_),
	card(NumberWinner,Seed,HierarchyWinner,_),
	Hierarchy > HierarchyWinner,
	!,
	fieldWinner(T,Winner,(Number,Seed,Player)).

fieldWinner([(Number,Seed,Player)|T],Winner,(NumberWinner,Seed,PlayerWinner)):-
	card(Number,Seed),
	card(NumberWinner,Seed),
	fieldWinner(T,Winner,(NumberWinner,Seed,PlayerWinner)),
	!.

% different seed
fieldWinner([(Number,Seed,Player)|T],Winner,(NumberWinner,SeedWinner,PlayerWinner)):-
	Seed \= SeedWinner,
	card(Number,Seed),
	card(NumberWinner,SeedWinner),
	currentBriscola(Seed),
	!,
	fieldWinner(T,Winner,(Number,Seed,Player)).

fieldWinner([(Number,Seed,Player)|T],Winner,(NumberWinner,SeedWinner,PlayerWinner)):-
	Seed \= SeedWinner,
	card(Number,Seed),
	card(NumberWinner,SeedWinner),
	fieldWinner(T,Winner,(NumberWinner,SeedWinner,PlayerWinner)).

% matchPoints(+Cards,-Points)
matchPoints(Cards,Points):- matchPoints(Cards,0,Points).

matchPoints([],Points,FinalPoints):-
	points(Points,TempPoints),
	FinalPoints is floor(TempPoints).

matchPoints([(Number,Seed)|T],CurrSum,Points):-
	card(Number,Seed,_,CardPoints),
	NewSum is CurrSum + CardPoints,
	matchPoints(T,NewSum,Points).

% totalPoints(+CardsFirstTeam,+CardsSecondTeam,+LastFieldWinner,-FirstTeamPoints,-SecondTeamPoints)
totalPoints(CardsFirstTeam,CardsSecondTeam,1,FirstTeamPoints,SecondTeamPoints):-
	lastTakeHasOneMorePoint,
	!,
	matchPoints(CardsFirstTeam,TempFirstTeamPoints),
	matchPoints(CardsSecondTeam,SecondTeamPoints),
	FirstTeamPoints is TempFirstTeamPoints + 1.

totalPoints(CardsFirstTeam,CardsSecondTeam,2,FirstTeamPoints,SecondTeamPoints):-
	lastTakeHasOneMorePoint,
	!,
	matchPoints(CardsFirstTeam,FirstTeamPoints),
	matchPoints(CardsSecondTeam,TempSecondTeamPoints),
	SecondTeamPoints is TempSecondTeamPoints + 1.

totalPoints(CardsFirstTeam,CardsSecondTeam,_,FirstTeamPoints,SecondTeamPoints):-
	matchPoints(CardsFirstTeam,FirstTeamPoints),
	matchPoints(CardsSecondTeam,SecondTeamPoints).

% matchWinner(+CardsFirstTeam,+CardsSecondTeam,+LastFieldWinner,-TeamWinner,-FirstTeamGainedPoints,-SecondTeamGainedPoints)
matchWinner(CardsFirstTeam,CardsSecondTeam,LastFieldWinner,1,FirstTeamGainedPoints,SecondTeamGainedPoints):-
	totalPoints(CardsFirstTeam,CardsSecondTeam,LastFieldWinner,FirstTeamPoints,SecondTeamPoints),
	FirstTeamPoints > SecondTeamPoints,
	!,
	winnerPoints(FirstTeamPoints,FirstTeamGainedPoints),
	loserPoints(SecondTeamPoints,SecondTeamGainedPoints).

matchWinner(CardsFirstTeam,CardsSecondTeam,LastFieldWinner,2,FirstTeamGainedPoints,SecondTeamGainedPoints):-
	totalPoints(CardsFirstTeam,CardsSecondTeam,LastFieldWinner,FirstTeamPoints,SecondTeamPoints),
	FirstTeamPoints < SecondTeamPoints,
	!,
	winnerPoints(SecondTeamPoints,SecondTeamGainedPoints),
	loserPoints(FirstTeamPoints,FirstTeamGainedPoints).

matchWinner(CardsFirstTeam,CardsSecondTeam,LastFieldWinner,0,GainedPoints,GainedPoints):-
	totalPoints(CardsFirstTeam,CardsSecondTeam,LastFieldWinner,Points,_),
	drawPoints(Points,GainedPoints).

% sessionWinner(+FirstTeamPoints,+SecondTeamPoints,-SessionWinner)
sessionWinner(FirstTeamPoints,SecondTeamPoints,1):-
	pointsToWinSession(Threshold),
	FirstTeamPoints >= Threshold,
	FirstTeamPoints > SecondTeamPoints,
	!.

sessionWinner(FirstTeamPoints,SecondTeamPoints,2):-
	pointsToWinSession(Threshold),
	SecondTeamPoints >= Threshold,
	SecondTeamPoints > FirstTeamPoints,
	!.
