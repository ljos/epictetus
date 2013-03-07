:- module(sandbox, [evaluate/2, read_lines/2]).

read_lines(Stream, [H|T]) :-
          read_line_to_codes(Stream, S),
          S \= end_of_file,
          string_to_atom(S, H),
          read_lines(Stream, T), !.
read_lines(_, []) :- !.

terms_to_atoms([], []).
terms_to_atoms([H|T], [F|R]) :-
    term_to_atom(H, F),
    terms_to_atoms(T, R).

:- open('whitelist', read, Stream),
   read_lines(Stream, Lines),
   terms_to_atoms(W, Lines),
   asserta(whitelist(W)),
   close(Stream).
:- compile_predicates([whitelist/1]).
whitelist(F, A) :-
    whitelist(Whitelist),
    member(F/A, Whitelist), !.

chk_whitelist([]).
chk_whitelist([H|T]) :-
    check_whitelist(H),
    chk_whitelist(T), !.

check_whitelist(Term) :-
    var(Term); atomic(Term); is_list(Term).
check_whitelist(Term) :-
    Term =.. [H|T],
    length(T, N),
    whitelist(H, N),
    chk_whitelist(T), !.

evaluate(Chars, Names) :-
    open_chars_stream(Chars, Stream),
    read_term(Stream, Term, [syntax_errors(quiet), variable_names(Names)]),
    close(Stream),
    check_whitelist(Term),
    call_with_time_limit(5, Term).
