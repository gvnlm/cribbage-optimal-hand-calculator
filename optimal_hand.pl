/** CRIBBAGE: OPTIMAL HAND CALCULATOR ******************************************

    For the game of Cribbage, takes dealt cards, and calculates which 4 cards
    (i.e., a hand) should be kept, and which cards should be discarded 
    (i.e., crib cards), in order to maximise the chances of winning.
    
    Approach: 
        1. Find all the possible hands you can make from the cards dealt.
        2. For each possible hand, calculate its expected value, that is, the 
           average amount of points scored over all possible start cards.
        3. Select the hand with the greatest expected value.

    Predicates are split into 3 categories:
        HAND PREDICATES   
            - hand_value/3              - pairs/3
            - hand_expected_value/3     - runs/3
            - select_hand/3             - flushes/3
            - fifteens/3                - one_for_his_nob/3
        
        CARD PREDICATES 
            - card/2                    - cards_values/2
            - suit/1                    - cards_orders/2
            - rank/3                    - order_counts/2
        
        HELPER PREDICATES  
            - combination_sum/3         - consecutive/1
            - combination/3             - product/2
            - elem_run_lens/2           - times/3
            - sublist/2                 - average/2  
            - between_desc/3
*/


/* HAND PREDICATES ************************************************************/


%% hand_value(+Hand, +StartCard, -Value) is det
%
%  Value is the total cribbage point value of Hand, when StartCard is the start 
%  card.
%  - Hand is a list of card(Rank, Suit) terms.
%  - StartCard is a card(Rank, Suit) term.

hand_value(Hand, StartCard, Value) :-
    fifteens(Hand, StartCard, P0),
    pairs(Hand, StartCard, P1),
    runs(Hand, StartCard, P2),
    flushes(Hand, StartCard, P3),
    one_for_his_nob(Hand, StartCard, P4),
    Value is P0+P1+P2+P3+P4.


%% hand_expected_value(+StartCards, +Hand, -ExpectedValue) is det
%
%  ExpectedValue is the average hand value of Hand over all start cards in
%  StartCards.
%  - Hand is a list of card(Rank, Suit) terms.
%  - StartCards is list a card(Rank, Suit) terms.

hand_expected_value(StartCards, Hand, ExpValue) :-
    maplist(hand_value(Hand), StartCards, Values),
    average(Values, ExpValue).


%% select_hand(+CardsDealt, -OptimalHand, -CribCards) is det
%
%  OptimalHand has the highest expected hand value out of all possible hand 
%  combinations of CardsDealt. CribCards are the cards in CardsDealt that are 
%  discarded (not kept to be apart of OptimalHand).
%  - CardsDealt is a list of N card(Rank, Suit) terms.
%  - OptimalHand is a list of 4 card(Rank, Suit) terms.
%  - CribCards is a list of N-4 card(Rank, Suit) terms.

select_hand(CardsDealt, OptimalHand, CribCards) :-
    findall(Hand, combination(CardsDealt, 4, Hand), Hands), 
    findall(card(Rank, Suit), card(Rank, Suit), Deck),
    subtract(Deck, CardsDealt, StartCards),
    map_list_to_pairs(hand_expected_value(StartCards), Hands, ExpValueHands), 
    max_member(_-OptimalHand, ExpValueHands), 
    subtract(CardsDealt, OptimalHand, CribCards).


%% fifteens(+Cards, -Points) is det
%
%  Points is the point value awarded to Hand for fifteens, when StartCard is
%  the start card.
%  - Hand is a list of card(Rank, Suit) terms.
%  - StartCard is a card(Rank, Suit) term.

fifteens(Hand, StartCard, Points) :-
    cards_values([StartCard|Hand], Values),
    findall(Combo, combination_sum(Values, 15, Combo), Combos),
    length(Combos, NumOfCombos),
    Points is 2*NumOfCombos.


%% pairs(+Cards, -Points) is det
%
%  Points is the point value awarded to Hand for pairs, when StartCard is the
%  start card.
%  - Hand is a list of card(Rank, Suit) terms.
%  - StartCard is a card(Rank, Suit) term.

pairs(Hand, StartCard, Points) :-
    order_counts([StartCard|Hand], OrderCounts),
    pairs_values(OrderCounts, Counts),
    % map each rank count to points for pairs
    findall(P, (member(C, Counts), P is C*(C-1)), Ps),
    sum_list(Ps, Points).


%% runs(+Cards, -Points) is det
% 
%  Points is the point value awarded to Hand for runs, when StartCard is the
%  start card.
%  - Hand is a list of card(Rank, Suit) terms.
%  - StartCard is a card(Rank, Suit) term.

runs(Hand, StartCard, Points) :-
    order_counts([StartCard|Hand], OrderCounts),
    % sublist searches solutions for OrderCountRun in descending length
    (sublist(OrderCounts, OrderCountRun),
     length(OrderCountRun, RunLen),
     RunLen >= 3,
     pairs_keys(OrderCountRun, Orders),
     consecutive(Orders) ->
        pairs_values(OrderCountRun, Counts),
        product(Counts, NumOfRuns),
        Points is RunLen*NumOfRuns
    ;
        Points = 0
    ).


%% flushes(+Cards, -Points) is det
%
%  Points is the point value awarded to Hand for flushes, when StartCard is the 
%  start card.
%  - Hand is a list of card(Rank, Suit) terms.
%  - StartCard is a card(Rank, Suit) term.

flushes(Hand, StartCard, Points) :-
    maplist(arg(2), Hand, Suits),
    (maplist(=(Suit), Suits) ->
        (StartCard = card(_, Suit) ->
            Points = 5
        ;
            Points = 4
        )
    ;
        Points = 0
    ).


%% one_for_his_nob(+Hand, +StartCard, -Points) is det
%
%  Points is the point value awarded to Hand for "One for his nob", when 
%  StartCard is the start card.
%  - Hand is a list of card(Rank, Suit) terms.
%  - StartCard is a card(Rank, Suit) term.

one_for_his_nob(Hand, StartCard, Points) :-
    (member(card(jack, Suit), Hand),
     StartCard = card(_, Suit) ->
        Points = 1
    ;
        Points = 0
    ).


/* CARD PREDICATES ************************************************************/


%% card(+Rank, +Suit) is semidet
%  card(+Rank, -Suit) is semidet
%  card(-Rank, +Suit) is semidet
%  card(-Rank, -Suit) is multi
%
%  Rank is a card rank, and Suit is a card suit.

card(Rank, Suit) :-
    suit(Suit),
    rank(Rank, _, _).


%% suit(+Suit) is semidet
%
%  Suit is a card suit.

suit(clubs).
suit(diamonds).
suit(hearts).
suit(spades).


%% rank(+Rank, ?Value, ?Order) is semidet
%
%  Rank is a card rank.
%  Value is the value of Rank in fifteens.
%  Order is the precedence of Rank, 1 being the lowest and 13 being the highest.

rank(ace, 1, 1).
rank(2, 2, 2).
rank(3, 3, 3).
rank(4, 4, 4).
rank(5, 5, 5).
rank(6, 6, 6).
rank(7, 7, 7).
rank(8, 8, 8).
rank(9, 9, 9).
rank(10, 10, 10).
rank(jack, 10, 11).
rank(queen, 10, 12).
rank(king, 10, 13).


%% cards_values(+Cards, -Values) is det
%
%  Values is a list of the rank values of cards in Cards.

cards_values(Cards, Values) :-
    % map each card to its rank value
    findall(Value, (member(card(Rank, _), Cards), rank(Rank, Value, _)),
            Values).


%% cards_orders(+Cards, -Orders) is det
%
%  Orders is a list of the rank orders of cards in Cards.

cards_orders(Cards, Orders) :-
    % map each card to its rank order
    findall(Order, (member(card(Rank, _), Cards), rank(Rank, _, Order)), 
            Orders).


%% order_counts(+Cards, -Pairs) is det
%  
%  Pairs is a list of Order-Count pairs that represents the count of rank 
%  orders of cards in Cards.
%  Pairs are sorted in ascending rank order.

order_counts(Cards, OrderCounts) :-
    cards_orders(Cards, Orders),
    msort(Orders, OrdersSorted),
    elem_run_lens(OrdersSorted, OrderCounts).


/* HELPER PREDICATES **********************************************************/


%% combination_sum(+List, +Sum, ?Combination) is nondet
%
%  Combination is a list representing a combination of some/all elements from
%  List, in which the sum of the combination equals Sum.

combination_sum(List, Sum, Combo) :-
    combination(List, _, Combo),
    sum_list(Combo, Sum).


%% combination(+List, ?SampleSize, ?Combination) is multi
%
%  Combination is a list representing a combination of SampleSize elements from 
%  List.

combination([], 0, []).
combination([X|Xs], ComboLen, Combo) :-
    (length([X|Xs], ListLen),
     between(1, ListLen, ComboLen),
     length(Combo, ComboLen),
     % include X in Combo
     Combo = [X|Combo0],
     Combo0Len is ComboLen-1,
     combination(Xs, Combo0Len, Combo0)
    ;
     % exclude X from Combo
     combination(Xs, ComboLen, Combo)
    ).


%% elem_run_lens(+List, -Pairs) is det
%
%  Pairs is a list of Elem-RunLen pairs that represents the run length of 
%  elements in List.
%  Example: elem_run_lens([3, 2, 2, 1, 1, 1], ElemRunLens).
%           ElemRunLens = [3-1, 2-2, 1-3].

elem_run_lens([], []).
elem_run_lens([X|Xs], ElemRunLens) :-
    elem_run_lens([X|Xs], 1, ElemRunLens).

elem_run_lens(List, XRunLen, ElemRunLens) :-
    (List = [X] ->
        ElemRunLens = [X-XRunLen]
    ;
        List = [X, Y|Ys],
        (Y = X ->
            YRunLen is XRunLen+1,
            elem_run_lens([Y|Ys], YRunLen, ElemRunLens)
        ;
            ElemRunLens = [X-XRunLen|ElemRunLens0],
            elem_run_lens([Y|Ys], 1, ElemRunLens0)
        )
    ).


%% sublist(+List, -SubList) is multi
% 
%  SubList is a sublist of List.
%  Solutions for SubList are searched in descending length order.
%  The empty list [] is considered a sublist of any list.

sublist([X|Xs], SubList) :-
    length([X|Xs], ListLen),
    between_desc(ListLen, 1, SubListLen),
    length(SubList, SubListLen), 
    between(SubListLen, ListLen, TailLen),
    length(Tail, TailLen),
    append(SubList, _, Tail),
    append(_, Tail, [X|Xs]).  
sublist(_, []).


%% between_desc(+High, +Low, -Value) is nondet
%
%  High >= Value >= Low, where High, Value and Low are integers.
%  Solutions for Value are searched from High to Low.

between_desc(Hi, Lo, X) :-
    NegHi is -Hi,
    NegLo is -Lo,
    between(NegHi, NegLo, NegX),
    X is -NegX.


%% consecutive(+List) is semidet
%
%  List's structure is either [Min, Min+1, ... High] or [OnlyElem].

consecutive(List) :-
    (List = [_] ->
        true
    ;
        List = [X, Y|Ys],
        Y is X+1,
        consecutive([Y|Ys])
    ).


%% product(+List, -Product) is det
%
%  Product is the product value of all numbers in List.

product(List, Product) :-
    foldl(times, List, 1, Product).


%% times(+X, +Y, -XY) is det
%
%  XY is X times Y.

times(X, Y, XY) :-
    XY is X*Y.


%% average(+List, -Average) is semidet
%
%  Average is the average value of all numbers in List.
%  divide by 0 error when List is empty.

average(List, Avg) :-
    sum_list(List, Sum),
    length(List, NumOfElems),
    Avg is Sum/NumOfElems.
