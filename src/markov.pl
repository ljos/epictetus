:- module(markov, [markov/1]).

space(C) :- code_type(C, space).
ok(C) :- \+ space(C).

blanks([C]) --> [C], { space(C) }, blanks.
blanks --> [].

inwords([C|Cs]) --> [C], { ok(C) }, inwords(Cs).
inwords([C]) --> [C], { ok(C) }.

split_str([H|T]) --> blanks, inwords(H), blanks, !, split_str(T).
split_str([], _, _).

split_codes([], []).
split_codes([H|T], Sentence) :-
    space(H),
    split_codes(T, Sentence).
split_codes(Codes, [Word | Sentence]) :-
    phrase(split_str([W]), Codes),
    append(W, Rest, Codes),
    atom_codes(Word, W),
    split_codes(Rest, Sentence).

read_lines(Stream, [Line|T]) :-
    read_line_to_codes(Stream, Line),
    Line \= end_of_file,
    !, read_lines(Stream, T).
read_lines(_, []) :- !.

file_to_lines(File, Lines) :-
    open(File, read, Stream),
    read_lines(Stream, Lines).

lines_to_atoms([], []).
lines_to_atoms([H|T], Atoms) :-
    split_codes(H, Sentence),
    lines_to_atoms(T, Rest),
    append(Sentence, Rest, Atoms).

end_of_sentence(Word) :-
    Ends = ['.', '!', '?'],
    member(End, Ends),
    atom_concat(_, End, Word), !.

:- dynamic(markov_edge/2).
:- dynamic(markov_start/1).

add_atoms([_]).
add_atoms([H,S|T]) :-
    \+ end_of_sentence(H),
    assert(markov_edge(H,S)),
    add_atoms([S|T]).
add_atoms([H,S|T]) :-
    end_of_sentence(H),
    assert(markov_start(S)),
    add_atoms([S|T]).

add_file(File, Atoms) :-
    file_to_lines(File, Lines),
    lines_to_atoms(Lines, Atoms).

:- add_file('epictetus.txt', Atoms), add_atoms(Atoms).

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
