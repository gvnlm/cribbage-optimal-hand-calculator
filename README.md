# About

For the game of [Cribbage](https://en.wikipedia.org/wiki/Cribbage), takes cards dealt, and calculates which 4 cards (i.e., a hand) should be kept, and which cards should be discarded (i.e., crib cards), in order to maximise the expected value of your hand.

# Usage

### 1. Start [SWI-Prolog](https://www.swi-prolog.org/Download.html)

Start a Prolog environment by running `swipl` in the terminal.

### 2. Load `optimal_hand.pl`

Inside your Prolog environment, run `[optimal_hand].`. This should return **`true.`** to indicate `optimal_hand.pl` has successfully been loaded.

### 3. Query the `select_hand(+CardsDealt, -OptimalHand, -CribCards)` predicate

For example,

```
?- select_hand([
    card(3, hearts),
    card(5, clubs),
    card(7, hearts),
    card(9, diamonds),
    card(9, spades),
    card(king, hearts)
],
OptimalHand,
CribCards).
```

returns

```
OptimalHand = [card(3, hearts), card(5, clubs), card(7, hearts), card(king, hearts)],
CribCards = [card(9, diamonds), card(9, spades)].
```
