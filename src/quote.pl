:- module(quote, [quote/1]).
:- use_module(sandbox).

:- dynamic(quote/1).
quote(Quote) :-
    open('quotes', read, Stream),
    read_lines(Stream, Quotes),
    asserta(quote(Q) :- random_member(Q, Quotes)),
    close(Stream),
    !, quote(Quote).
