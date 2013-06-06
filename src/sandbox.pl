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

chk_whitelist([], []).
chk_whitelist([H|T], [Term|Tail]) :-
    check_whitelist(H, Term),
    chk_whitelist(T, Tail), !.

check_whitelist(Term, Term) :-
    var(Term); atomic(Term); is_list(Term).
check_whitelist(Term, Predicate) :-
    Term =.. [Name | Parameters],
    length(Parameters, N),
    whitelist(Name, N),
    chk_whitelist(Parameters, SafeParams),
    Predicate =.. [Name | SafeParams].
check_whitelist(Term, Predicate) :-
    Term =.. [Name | Parameters],
    atom_concat(sandbox_, Name, SafeName),
    chk_whitelist(Parameters, SafeParams),
    Predicate =.. [SafeName | SafeParams].

underscore(V) :-
    V =.. [=, F, _],
    string_concat('_', _, F).

handle_error(error(syntax_error(_), _), [error(syntax_error)]).
handle_error(error(existence_error(_, _), _), [error(existence_error)]).
handle_error(time_limit_exceeded, [error(time_limit_exceeded)]).
handle_error(Error, [Error]).

evaluate(Chars, Variables) :-
    catch((open_chars_stream(Chars, Stream),
           read_term(Stream, Term, [variable_names(Names)]),
           (at_end_of_stream(Stream);
            throw(error(not_at_end_of_stream))),
           close(Stream),!,
           check_whitelist(Term, SafePredicate),
           call_with_time_limit(1, SafePredicate),
           exclude(underscore, Names, Variables)),
          Error,
          handle_error(Error, Variables)), !.
