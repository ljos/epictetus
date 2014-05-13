:- module(markov, [markov/1]).
:- use_module(library(dcg/basics)).

:- dynamic(markov_edge/2).
:- dynamic(markov_start/1).

punct([P|S], S) :-
    char_type(P, period).

char(C) -->
    nonblank(C),
    {
     not(char_type(C, period))
    }.

chars([C | Cs]) --> char(C), chars(Cs), !.
chars([C]) --> char(C).

sentence([]) --> punct.
sentence([Word | Words]) -->
    chars(Chars), blanks, sentence(Words),
    {
     atom_codes(Word, Chars)
    }.

sentences([]) --> eos.
sentences([S]) --> sentence(S).
sentences([S | Ss]) --> sentence(S), blanks, sentences(Ss).

assert_chain(W1, W0, W1) :-
    assert(markov_edge(W0, W1)).

assert_markov([]).
assert_markov([Start|Rest]) :-
    assert(markov_start(Start)),
    foldl(assert_chain, Rest, Start, _).

add_file(File) :-
    read_file_to_codes(File, Codes, []),
    phrase(sentences(Sentences), Codes),!,
    maplist(assert_markov, Sentences).

:- add_file('epictetus.txt').

markov(Sentence) :-
    bagof(S, markov_start(S), Starts),
    random_member(Start, Starts),
    markov(Start, List),
    atomic_list_concat(List, ' ', Sentence).
markov(Word, [Word|Rest]) :-
    bagof(N, markov_edge(Word, N), Nexts),
    random_member(Next, Nexts),
    markov(Next, Rest).
markov(Word, [Word]).
