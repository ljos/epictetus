:- module(epictetus, []).
:- use_module(library(dcg/basics)).
:- use_module(sandbox).
:- consult(config).

:- volatile(read_stream/2).
:- volatile(write_stream/2).

connect(Host:Port) :-
    tcp_socket(Socket),
    tcp_connect(Socket, Host:Port),
    tcp_open_socket(Socket, INs, OUTs),
    assert(read_stream(Host:Port, INs)),
    assert(write_stream(Host:Port, OUTs)),
    write('Connected to '),
    write(Host),
    write(' on port '),
    write(Port),
    writeln('.'), !.

write_to_stream(String) :-
    server(_, host(Host), _),
    write_stream(Host, OStream),
    write_to_stream(OStream, String).
write_to_stream(Stream, String) :-
    atomic(String),
    write(Stream, String),
    nl(Stream),
    flush_output(Stream).
write_to_stream(Stream, String) :-
    swritef(Out, '%s', [String]),
    write_to_stream(Stream, Out).

send_info(Nick) :-
    append("NICK ", Nick, MsgNick),
    write_to_stream(MsgNick),
    append("USER ", Nick, A),
    append(A, " 0 * :", B),
    append(B, Nick, MsgUser),
    write_to_stream(MsgUser), !.

join_channel(Channel) :-
    server(_, host(Host), _),
    write_stream(Host, OStream),
    append("JOIN ", Channel, MsgJoin),
    write_to_stream(OStream, MsgJoin).

write_to_channel(Channel, String) :-
    append("PRIVMSG ", Channel, A),
    append(A, " :", B),
    append(B, String, Msg),
    write_to_stream(Msg).

write_variables_to(_, [error(syntax_error)]).
write_variables_to(_, [error(existence_error)]).
write_variables_to(_, [error(not_at_end_of_stream)]).
write_variables_to(_, [error(time_limit_exceeded)]).
write_variables_to(Channel, [error(instantiation_error,_)]) :-
    maybe(0.01),
    write_to_channel(Channel, "... 1,000,000 ............ 10,000,000 years later"),
    write_to_channel(Channel, "      >> 42 << (last release gives the question)").
write_variables_to(_, [error(_,_)]).
write_variables_to(Channel, []) :-
    write_to_channel(Channel, "Yes.").
write_variables_to(Channel, [H]) :-
    swritef(S, '%w', [H]),
    atom_codes(S, String),
    write_to_channel(Channel, String).
write_variables_to(Channel, [H|T]) :-
    swritef(S, '%w', [H]),
    atom_codes(S, C),
    append(C, ",", String),
    write_to_channel(Channel, String),
    write_variables_to(Channel, T).

command(msg(_, _, Command)) :-
    server(_, _, channel(Channel)),
    parse_message(Command, Response),
    write_to_channel(Channel, Response).
command(msg(_, _, Command)) :-
    server(_, _, channel(Channel)),
    evaluate(Command, Vars),
    write_variables_to(Channel, Vars).
command(msg(_, _, Command)) :-
% if evaluate fails we should return No.
% does not fail on syntax_error or timeout,
% those are handled by write_variables_to.
    server(_, _, channel(Channel)),
    write_to_channel(Channel, "No.").

%      P  O  N  G ' ' :
ping([80,79,78,71,32,58|Value]) --> "PING :", nonblanks(Value).

message(msg(From, To, Message)) -->
    ":", string(From), "!", string(_),
    blank, "PRIVMSG", blank, string(To), blank,
    ":", string(Message).

connection --> string(_), "+ix", string(_).

respond(Request) :-
    phrase(ping(Pong), Request),
    write_to_stream(Pong).
respond(Request) :-
    phrase(connection, Request),
    server(_, _, channel(Channel)),
    join_channel(Channel).
respond(Request) :-
    phrase(message(Message), Request),
    command(Message).

read_irc :-
    server(_, host(Host), _),
    read_stream(Host, IStream),
    read_line_to_codes(IStream, In),
    ignore(respond(In)),
    read_irc.

close_streams(Host) :-
    write_stream(Host, OStream),
    close(OStream),
    read_stream(Host, IStream),
    close(IStream).

irc_connect :-
    server(nick(Nick), host(Host), _),
    connect(Host),
    send_info(Nick),
    thread_create(catch(read_irc,_, write('Read aborted.')),
                  _,
                  [alias(read_irc_thread),
                   detached(true),
                   at_exit(close_streams(Host))]).

save :-
    qsave_program('epictetus').
