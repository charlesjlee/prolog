tap(From, To, ToNew) :-
	From \= 0,
	Total is From + To,
	ToNew is mod(Total, 5).

splits(0, 4, 2).
splits(4, 0, 2).
splits(0, 2, 1).
splits(2, 0, 1).

lost(0, 0).

% get_next(hand(TopLeft,TopRight,BottomLeft,BottomRight,Turn), Next, Seen)
% calculates the Next unSeen state given it is Turn's move
get_next(hand(TL,TR,BL,BR,bottom), Next, Seen) :- tap(BL,TL,TL_New), Next=hand(TL_New,TR,BL,BR,top), \+member(Next,Seen).
get_next(hand(TL,TR,BL,BR,bottom), Next, Seen) :- tap(BR,TR,TR_New), Next=hand(TL,TR_New,BL,BR,top), \+member(Next,Seen).
get_next(hand(TL,TR,BL,BR,bottom), Next, Seen) :- tap(BL,TR,TR_New), Next=hand(TL,TR_New,BL,BR,top), \+member(Next,Seen).
get_next(hand(TL,TR,BL,BR,bottom), Next, Seen) :- tap(BR,TL,TL_New), Next=hand(TL_New,TR,BL,BR,top), \+member(Next,Seen).
get_next(hand(TL,TR,BL,BR,top), Next, Seen) :- tap(TL,BL,BL_New), Next=hand(TL,TR,BL_New,BR,bottom), \+member(Next,Seen).
get_next(hand(TL,TR,BL,BR,top), Next, Seen) :- tap(TR,BR,BR_New), Next=hand(TL,TR,BL,BR_New,bottom), \+member(Next,Seen).
get_next(hand(TL,TR,BL,BR,top), Next, Seen) :- tap(TR,BL,BL_New), Next=hand(TL,TR,BL_New,BR,bottom), \+member(Next,Seen).
get_next(hand(TL,TR,BL,BR,top), Next, Seen) :- tap(TL,BR,BR_New), Next=hand(TL,TR,BL,BR_New,bottom), \+member(Next,Seen).

get_next(hand(TL,TR,BL,BR,bottom), Next, Seen) :- splits(BL,BR,S), Next=hand(TL,TR,S,S,top), \+member(Next,Seen).
get_next(hand(TL,TR,BL,BR,top), Next, Seen) :- splits(TL,TR,S), Next=hand(S,S,BL,BR,bottom), \+member(Next,Seen).

% current player already won
forced_win(hand(TL,TR,_,_,bottom), _) :- lost(TL,TR), !.
forced_win(hand(_,_,BL,BR,top), _) :- lost(BL, BR), !.

% current player can win in a single move
forced_win(hand(TL,TR,BL,BR,bottom), Seen) :-
	\+ lost(BL, BR),
	get_next(hand(TL,TR,BL,BR,bottom), hand(TL_2,TR_2,BL,BR,top), Seen),
	lost(TL_2, TR_2),
	!.
forced_win(hand(TL,TR,BL,BR,top), Seen) :-
	\+ lost(TL, TR),
	get_next(hand(TL,TR,BL,BR,top), hand(TL,TR,BL_2,BR_2,bottom), Seen),
	lost(BL_2, BR_2),
	!.

% current player has a branch that always ends in a win
forced_win(H1, Seen) :-
	% can the current player choose a move...
	get_next(H1, H2, Seen),
	append(Seen, H1, NewSeen),

	% s.t. all of the opponent's possible moves result in a forced win for the current player?
	bagof(H3, get_next(H2, H3, NewSeen), Hs),
	forall(member(H, Hs), forced_win(H, NewSeen)),
	format('winning move: ~w~n', H2), % the winning move
	!.