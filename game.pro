location(home, "You are right next to your wooden cabin").
location(forest, "You are on the path going through forest").
location(wagon, "You are in the forest near flipped over wagon").
location(town, "You are in the town hall").
location(smithy, "You are at smithy, a blacksmith's shop").
location(jewlers, "You are at jewlers shop").
location(village, "You are in the village right outside of the town gates").
location(valley, "You are in the vally").
location(meadow, "You are on the meadow").
location(cave,
"You are in the cave. As you hear your wife scream, you rush forward until enormous,
green creature emerges in front of you. The ogre turns around and charge towards you.").

description(home,
"There is a forest on the left and meadow on the right.").
description(forest,
"There is a path in front of you leading to town, on the right
you see flipped over mearchant wagon peeking through the woods.").
description(wagon,
"Upon further investigation the merchant wagon seems to be empty,
you don't see anything particular here.").
description(town,
"It's a busy place. There is a plenty of people going around. Headly reek and tremendous
noice reminds you why you live in the forest. Through the crowd you see a blacksmith's shop
on the right and jewlers on the left, there is a town gate leading to nearby village in front of you.").
description(smithy,
"You see a great sword for sale, ideal weapon for facing a big monster. It costs 90 coins.").
description(jewlers,
"You see many silver, gold jewlery and ornaments. You could sell precious metals here.").
description(village,
"It is a poor village. You see old farmer waving to you, you can talk to him. There is also
a valley in front of you, that streches away to the horizon.").
description(valley,
"At the bottom of valley you see a chicken playing in the river, looking curiously at you.").
description(meadow,
"You see large footprints leading to the nearby cave, which is in front of you.").

path(home, forest, left).
path(home, meadow, right).
path(forest, home, back).
path(forest, wagon, right).
path(wagon, forest, back).
path(forest, town, forward).
path(town, forest, back).
path(town, smithy, right).
path(smithy, town, back).
path(town, jewlers, left).
path(jewlers, town, back).
path(town, village, forward).
path(village, town, back).
path(village, valley, forward).
path(valley, village, back).
path(meadow, home, back).
path(meadow, cave, forward).

:- dynamic position/2.
:- dynamic equipment/1.
:- dynamic money/1.
:- dynamic description/2.

position(player, home).
position(chicken, valley).
position(goldenEgg, villager).
position(sword, smithy).
equipment([]).
money(10).

printItems([Head | Tail]) :- write(Head), nl, printItems(Tail).

isListEmpty(List) :- not(member(_,List)).

money :- money(CurrentBalance), writeln(CurrentBalance).

items :-
    equipment(Items),
    not(isListEmpty(Items)),
    writeln("Inventory:"),
    assert(pi(true)),
    printItems(Items).

items :-
    equipment(Items),
    isListEmpty(Items),
    writeln("Inventory is empty").

talk :-
    position(player, village),
    position(chicken, valley),
    writeln("Farmer tells you he lost his favourite chicken Lucy and he can't find her."),
    writeln("He heard people saw her last time in the vally. He offers you a reward if you"),
    writeln("manage to bring her to home."), !.

talk :-
    position(player, village),
    position(chicken, player),
    writeln("Farmer cheerfully raise hands as you give him a chicken."),
    writeln("He thanks you and gives you a golden egg in return."),

    equipment(Items),
    append([goldenEgg], Items, AfterAddItems),
    delete(AfterAddItems, chicken, NewItems),
    retractall(equipment(_)),
    assert(equipment(NewItems)),

    retract(position(chicken, player)),
    assert(position(chicken, villager)),
    retract(position(goldenEgg, villager)),
    assert(position(goldenEgg, player)), !.

talk :-
    position(player, village),
    position(chicken, villager),
    writeln("Farmer greets you as he pets his chicken."), !.

talk :-
    writeln("Cannot talk to anyone here").

take :-
    position(player, valley),
    position(chicken, valley),
    writeln("You grab a chicken"),

    equipment(Items),
    append([chicken], Items, NewItems),
    retractall(equipment(_)),
    assert(equipment(NewItems)),

    description(valley, Description),
    retract(description(valley, Description)),
    assert(description(valley, "You don't see anything particular here beside river and hills.")),

    retract(position(chicken, valley)),
    assert(position(chicken, player)), !.

take :-
    writeln("Cannot take anything here").

sell :-
    position(player, jewlers),
    position(goldenEgg, player),
    writeln("You sell a golden egg for 100 coins"),

    equipment(Items),
    delete(Items, goldenEgg, NewItems),
    retractall(equipment(_)),
    assert(equipment(NewItems)),

    money(CurrentBalance),
    NewBalance is CurrentBalance + 100,
    retract(money(CurrentBalance)),
    assert(money(NewBalance)),

    retract(position(goldenEgg, player)),
    assert(position(goldenEgg, jewlers)), !.

sell :-
    writeln("Cannot sell anything").

buy :-
    position(player, smithy),
    position(sword, smithy),
    money(CurrentBalance),
    (CurrentBalance >= 90 ->
        writeln("You buy a great sword for 90 coins"),

        equipment(Items),
        append([sword], Items, NewItems),
        retractall(equipment(_)),
        assert(equipment(NewItems)),

        money(CurrentBalance),
        NewBalance is CurrentBalance - 90,
        retract(money(CurrentBalance)),
        assert(money(NewBalance)),

        description(smithy, Description),
        retract(description(smithy, Description)),
        assert(description(smithy, "You don't see more weapons for sale.")),

        retract(position(sword, smithy)),
        assert(position(sword, player))
        ;
        writeln("You don't have enough money to buy a great sword")
    ), !.

buy :-
    writeln("Cannot buy anything").

faceOgre :-
    position(player, cave),
    position(sword, player),
    retract(position(player, cave)),
    assert(position(player, end)),
    writeln("You pull out a sword and at the last secound stick it out in front of you."),
    writeln("You ran the monster through with a sword. The ogre freezes and falls on the ground."),
    writeln("Your ran out of the cave with your wife and you go back home together. Happy days!"), nl, !.

faceOgre :-
    position(player, cave),
    retract(position(player, cave)),
    assert(position(player, end)),
    writeln("Since you don't have any weapon, the ogre shreads you into pieces. You die."), nl, !.

faceOgre.

where :- position(player, CurrentPosition), writeln(CurrentPosition).

look :-
    position(player, CurrentPosition),
    description(CurrentPosition, Description),
    writeln(Description).

go(Direction) :-
    position(player, CurrentPosition),
    path(CurrentPosition, Next, Direction),
    retract(position(player, CurrentPosition)),
    assert(position(player, Next)),
    location(Next, LocationDescription),
    writeln(LocationDescription), !.

go(_) :-
    writeln("Cannot go there").

printCurrentLocation :-
    position(player, CurrentPosition),
    location(CurrentPosition, Description),
    writeln(Description).

printInstructions :-
    nl,
    writeln("Instructions:"),
    writeln("forward.    -       move forward"),
    writeln("back.       -       move back"),
    writeln("left.       -       move left"),
    writeln("right.      -       move right"),
    writeln("look.       -       look around"),
    writeln("talk.       -       talk to somebody"),
    writeln("take.       -       take item"),
    writeln("buy.        -       buy item"),
    writeln("sell.       -       sell item"),
    writeln("money.      -       see number of coins"),
    writeln("items.      -       see inventory").

printStory :-
    nl,
    writeln("Loud bashing to the your door wakes you up. You quckly jump out of bed to open them."),
    writeln("Turns out it is your neighbour who lives in the cabin on the other side of the river"),
    writeln("He tells you that gigantic ogre kiddnaped your wife when she was picking up flowers"),
    writeln("on the nearby meadow. You rush out of the house in wrath but realise you don't have"),
    writeln("any weapon to fight the monster, nor enough money to buy it.").

handleInput(forward) :-
    go(forward), !.

handleInput(back) :-
    go(back), !.

handleInput(left) :-
    go(left), !.

handleInput(right) :-
    go(right), !.

handleInput(look) :-
    look, !.

handleInput(take) :-
    take, !.

handleInput(talk) :-
    talk, !.

handleInput(buy) :-
    buy, !.

handleInput(sell) :-
    sell, !.

handleInput(money) :-
    money, !.

handleInput(items) :-
    items, !.

handleInput(_) :-
    pi(true),
    retract(pi(true)), !.

handleInput(_) :-
    writeln("Unknown move").

setup :-
    retractall(position(_,_)),
    retractall(equipment(_)),
    retract(money(_)),
    retract(description(valley, _)),
    retract(description(smithy, _)),
    assert(description(smithy,
           "You see a great sword for sale, ideal weapon for facing a big monster. It costs 90 coins.")),
    assert(description(valley,
           "At the bottom of valley you see a chicken playing in the river, looking curiously at you.")),
    assert(position(player, home)),
    assert(position(chicken, valley)),
    assert(position(goldenEgg, villager)),
    assert(position(sword, smithy)),
    assert(equipment([])),
    assert(money(10)).

loop :-
    position(player, end),
    writeln("Game over"),
    writeln("Thanks for playing!"), !.

loop :-
    nl,
    read(Input),
    nl,
    call(handleInput(Input)),
    faceOgre,
    loop.

start :-
    setup,
    printInstructions,
    printStory,
    nl,
    printCurrentLocation,
    loop.