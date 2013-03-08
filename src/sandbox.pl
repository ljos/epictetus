:- module(sandbox, [evaluate/2, read_lines/2]).
:- use_module([quote]).

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

:- dynamic(fact/1).
fact(man(epictetus)).

check_whitelist(Term) :-
    var(Term); atomic(Term); is_list(Term).
check_whitelist(Term) :-
    Term =.. [assert | [A]],
    A =.. [Fact | _],
    assert(fact(Fact)).
check_whitelist(Term) :-
    Term =.. [Fact | _],
    fact(Fact).
check_whitelist(Term) :-
    Term =.. [H|T],
    length(T, N),
    whitelist(H, N),
    chk_whitelist(T), !.
check_whitelist(Term) :-
    Term =.. [H|T],
    length(T, N),
    throw(security_error(H/N)).

underscore(V) :-
    V =.. [=, F, _],
    string_concat('_', _, F).

handle_error(error(syntax_error(_), _), [error(syntax_error)]).
handle_error(error(existence_error(_, _), _), [error(existence_error)]).
handle_error(time_limit_exceeded, [error(time_limit_exceeded)]).

evaluate(Chars, Variables) :-
    catch((open_chars_stream(Chars, Stream),
           read_term(Stream, Term, [variable_names(Names)]),
           close(Stream),
           check_whitelist(Term),
           call_with_time_limit(1, Term),
           exclude(underscore, Names, Variables)),
          Error,
          (handle_error(Error, Variables), !)).
