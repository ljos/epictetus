:- module(epictetus, []).
:- use_module(sandbox).

server(nick("epictetus"),
       host('irc.codetalk.io'),
       port(6667),
       channel("#lobby")).

connect(Host, Port) :-
    tcp_socket(Socket),
    tcp_connect(Socket, Host:Port),
    tcp_open_socket(Socket, INs, OUTs),
    assert(connectedReadStream(INs)),
    assert(connectedWriteStream(OUTs)),
    write('Connected to '),
    write(Host),
    write(' on port '),
    write(Port),
    writeln('.'), !.

write_to_stream(String) :-
    connectedWriteStream(OStream),
    write_to_stream(OStream, String).
write_to_stream(Stream, String) :-
    atomic(String),
    write(Stream, String),
    nl(Stream),
    flush_output(Stream),
    writef("Sent:     %t\n", [String]).
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
    connectedWriteStream(OStream),
    append("JOIN ", Channel, MsgJoin),
    write_to_stream(OStream, MsgJoin).

write_to_channel(Channel, String) :-
    append("PRIVMSG ", Channel, A),
    append(A, " :", B),
    append(B, String, Msg),
    write_to_stream(Msg).

write_variables_to(Channel, []) :-
    write_to_channel(Channel, "Yes.").
write_variables_to(Channel, [error(syntax_error)]).
write_variables_to(Channel, [H]) :-
    swritef(S, '%w', [H]),
    atom_codes(S, C),
    append(C, ".", String),
    write_to_channel(Channel, String).
write_variables_to(Channel, [H|T]) :-
    swritef(S, '%w', [H]),
    atom_codes(S, C),
    append(C, ",", String),
    write_to_channel(Channel, String),
    write_variables_to(Channel, T).
command(Command) :-
    server(_, _, _, channel(Channel)),
    (evaluate(Command, Vars),
     write_variables_to(Channel, Vars);
     % if evaluate fails we should return No.
     % does not fail on syntax_error or timeout,
     % those are handled by write_variables_to.
     write_to_channel(Channel, "No.")).

respond(Request) :-
    append("PING :", Value, Request),
    append("PONG :", Value, Msg),
    write_to_stream(Msg).
respond(Request) :-
    append(_, ":+ix", Request),
    server(_, _, _, channel(Channel)),
    join_channel(Channel).
respond(Request) :-
    append(A, B, Request),
    append(_, "PRIVMSG", A),
    append(_ , ":", C),
    append(C, Command, B),
    command(Command).

read_irc :-
    connectedReadStream(IStream),
    read_line_to_codes(IStream, In),
    writef("Recieved: %s\n", [In]),
    ignore(respond(In)),
    read_irc.

irc_connect :-
    server(nick(Nick), host(Host), port(Port), _),
    connect(Host, Port),
    send_info(Nick),
    thread_create(read_irc, _, [alias(read_irc_thread)]).

close :-
    connectedWriteStream(OStream),
    close(OStream),
    connectedReadStream(IStream),
    close(IStream).
