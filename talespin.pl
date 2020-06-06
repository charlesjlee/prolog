/*
Run like this:
generate_story(_{name:"John",gender:male}, Story).

TODO
-----------------
execute_actions/5
- avoid OR
- use difference list instead of append

more flavor
- DECIDE to make a sandwich
- more foods
- more events
- event/food/descriptor types
- avoid repetition
- "then", "after that", "finally"

cleanup
- add docs
- break into modules

ideas
- add more sigma-states
- sigma-states can arise/worsen spontaneously
*/

generate_story(Input, Story) :-
	initialize(Input, Curr, Target),
	generate_opener(Curr, Target, Opener),

	get_actions(Curr, Target, Actions),
	execute_actions(Curr, Target, Actions, Descriptions),
	format('Descriptions: ~w~n~n', [Descriptions]),

	concatenate(Descriptions, ActionStrings),
	string_concat(Opener, ActionStrings, StoryWithOpener),
	generate_closer(Curr, Target, Closer),
	string_concat(StoryWithOpener, Closer, Story),
	!.

:- dynamic name/1.
:- dynamic pronoun/1.
:- dynamic possessive/1.
initialize(Input, Curr, Target) :-
	Input = _{name:Name, gender:Gender},
	asserta(name(Name)),
	assert_pronouns(Gender),
	Curr = [hungry],
	Target = [happy].

assert_pronouns(male) :- asserta(pronoun(he)), asserta(possessive(his)).
assert_pronouns(female) :- asserta(pronoun(she)), asserta(possessive(her)).

generate_opener(_Curr, _Target, Opener) :-
	name(Name),
	pronoun(Pronoun),
	first_char_uppercase(Pronoun, Upper),
	atomics_to_string(['One day, ',Name,' was in the kitchen and ',Pronoun,' was hungry. ',Upper,' decided to make a BLT sandwich. '], Opener).

% https://stackoverflow.com/questions/20297765/converting-1st-letter-of-atom-in-prolog
first_char_uppercase(WordLC, WordUC) :-
    atom_chars(WordLC, [FirstChLow|LWordLC]),
    atom_chars(FirstLow, [FirstChLow]),
    upcase_atom(FirstLow, FirstUpp),
    atom_chars(FirstUpp, [FirstChUpp]),
    atom_chars(WordUC, [FirstChUpp|LWordLC]).

generate_closer(_Curr, _Target, Closer) :-
	name(Name),
	possessive(Possessive),
	atomics_to_string(['With a full belly, ',Name,' could now get on with the rest of ',Possessive,' day. THE END'], Closer).

get_actions(Curr, Target, Actions) :-
	length(Actions, _L),
	phrase(plan(Curr, Target), Actions),
	!.

plan(Curr, Target) --> {subset(Target, Curr)}, !.
plan(Curr, Target) --> 
	{action(Curr, Next, Action)},
	[Action],
	plan(Next, Target).

execute_actions(Curr, Target, Actions, Final) :- execute_actions(Curr, Target, Actions, [], Final).
execute_actions(_, _, [], Accumulator, Final) :- format('no more actions!~n'), Accumulator = Final.
execute_actions(Curr, Target, [A|As], Accumulator, Final) :-
	random(1, 11, R),
	try_action(R, A, Curr, Next, Target, TargetNew, Description, Success),
	append(Accumulator, [Description], NewAccumulator),
	(
		(
			Success = true,
			format('action succeeded! ~w~n', [A]),

			% execute next action
			execute_actions(Next, TargetNew, As, NewAccumulator, Final)
		)
		;
		(
			Success = false,
			format('action failed! ~w~n', [A]),

			% replan and try again
			get_actions(Next, TargetNew, Actions),
			execute_actions(Next, TargetNew, Actions, NewAccumulator, Final)
		)
	),
	!.

try_action(R, takeout(Item, Location), Curr, Next, Target, Target, Description, true) :-
	R =< 7,
	name(Name),
	qualifier(Item, Qualifier),
	atomics_to_string([Name,' took ',Qualifier,' ',Item,' out of the ',Location,'. '], Description),
	append(Curr, [holding(Item)], Next).

try_action(R, takeout(Item, Location), Curr, Curr, Target, Target, Description, false) :-
	R =< 8,
	name(Name),
	qualifier(Item, Qualifier),
	atomics_to_string([Name,' went to take ',Qualifier,' ',Item,' out of the ',Location,', but dropped it on the floor. '], Description).

try_action(_, takeout(Item, Location), Curr, Curr, Target, Target, Description, false) :-
	name(Name),
	qualifier(Item, Qualifier),
	atomics_to_string([Name,' took ',Qualifier,' ',Item,' out of the ',Location,', but it was moldy and spoiled. '], Description).

try_action(_, make(Food, using(Ingredients)), Curr, Next, Target, Target, Description, true) :-
	pronoun(Pronoun),

	atoms_to_english_list(Ingredients, IngredientsString),
	atomics_to_string(['Then ',Pronoun,' used the ',IngredientsString,' to make a ',Food,'. '], Description),
	maplist(holding, Ingredients, HoldingIngredients),
	subtract(Curr, HoldingIngredients, CurrUsed),
	append(CurrUsed, [holding(Food)], Next).

try_action(R, eat(Food), Curr, Next, Target, Target, Description, true) :-
	name(Name),
	possessive(Possessive),

	R =< 9,
	atomics_to_string(['Finally, careful not to drop anything on the floor, ',Name,' ate ',Possessive,' ',Food,' - the whole, entire thing. '], Description),
	subtract(Curr, [holding(Food), hungry], Curr2),
	append(Curr2, [happy], Next).

try_action(_, eat(Food), Curr, Next, Target, Target, Description, false) :-
	name(Name),
	possessive(Possessive),
	pronoun(Pronoun),

	atomics_to_string([Name,' took a big bite out of ',Possessive,' ',Food,'. Too big. It slipped out of his hand onto the floor. Great. Now ',Pronoun, ' had to start all over. '], Description),
	subtract(Curr, [holding(Food)], Next).

atoms_to_english_list(Atoms, String) :-
	last(Atoms, Last),
	atomics_to_string(['and ', Last], LastAnd),
	select(Last, Atoms, LastAnd, AtomsWithAnd), % replace last element with 'and last'
	atomic_list_concat(AtomsWithAnd, ', ', AtomList),
	atom_string(AtomList, String).

% https://stackoverflow.com/a/33727012
concatenate(StringList, StringResult) :-
    maplist(atom_chars, StringList, Lists),
    append(Lists, List),
    atom_chars(StringResult, List).

qualifier(tomato, a).
qualifier(bacon, some).
qualifier(lettuce, some).
qualifier(bread, some).

inside(fridge, tomato).
inside(fridge, bacon).
inside(fridge, lettuce).
inside(pantry, bread).

recipe(sandwich, [tomato,bacon,lettuce,bread]).

eat(sandwich).

holding(X, Y) :- Y = holding(X). % wrapper for maplist

% generate takeout(Item, Location)
action(Curr, Next, Action) :-
	inside(Location, Item),
	append(Curr, [holding(Item)], Next),
	Action = takeout(Item, Location).

% generate make(Food, using(Ingredients))
action(Curr, Next, Action) :-
	recipe(Food, Ingredients),
	maplist(holding, Ingredients, HoldingIngredients),
	subset(HoldingIngredients, Curr),
	subtract(Curr, HoldingIngredients, CurrUsed),
	append(CurrUsed, [holding(Food)], Next),
	Action = make(Food, using(Ingredients)).

% generate eat(Food)
action(Curr, Next, Action) :-
	eat(Food),
	member(holding(Food), Curr),
	subtract(Curr, [holding(Food), hungry], Curr2),
	append(Curr2, [happy], Next),
	Action = eat(Food).
