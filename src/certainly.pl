nick("epictetus").
server(host('irc.codetalk.io'), port(6667), channel("#lobby")).

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
    writeln('.').

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
    write_to_stream(MsgUser).

irc_connect :-
    server(host(Host), port(Port), _),
    connect(Host, Port),
    nick(Nick),
    send_info(Nick).

join_channel(Channel) :-
    connectedWriteStream(OStream),
    append("JOIN ", Channel, MsgJoin),
    write_to_stream(OStream, MsgJoin).

write_to_channel(Channel, String) :-
    append("PRIVMSG ", Channel, A),
    append(A, " :", B),
    append(B, String, Msg),
    write_to_stream(Msg).

write_variable_to(Channel, []) :-
    write_to_channel(Channel, "Yes.").
write_variable_to(Channel, [H]) :-
    swritef(Var, '%w', [H]),
    string_concat(Var, '.', S),
    string_to_list(S, List),
    write_to_channel(Channel, List).
write_variable_to(Channel, [H|T]) :-
    swritef(Var, '%w', [H]),
    string_concat(Var, ',', S),
    string_to_list(S, List),
    write_to_channel(Channel, List),
    write_variable_to(Channel, T).

command(Command) :-
    append("share a quote", _, Command),
    server(_, _, channel(Channel)),
    write_to_channel(Channel, "You are a little soul carrying around a corpse.").
command(Command) :-
    append("evaluate ", Chars, Command),
    server(_, _, channel(Channel)),
    read_term_from_chars(Chars, Term, [syntax_errors(quiet), variable_names(Names)]),
    call(Term),
%    write('YES').
     write_variable_to(Channel, Names).
command(Command) :-
    append(_ , _, Command),
    server(_, _, channel(Channel)),
%    write('No.').
    write_to_channel(Channel, "No.").

respond(Request) :-
    append("PING :", Value, Request),
    append("PONG :", Value, Msg),
    write_to_stream(Msg).
respond(Request) :-
    append(_, ":+ix", Request),
    server(_, _, channel(Channel)),
    join_channel(Channel).
respond(Request) :-
    append(_, B, Request),
    append(":epictetus, ", Command, B),
    command(Command).

read_irc :-
    connectedReadStream(IStream),
    read_line_to_codes(IStream, In),
    writef("Recieved: %s\n", [In]),
    ignore(respond(In)),
    read_irc.

close :-
    connectedWriteStream(OStream),
    close(OStream),
    connectedReadStream(IStream),
    close(IStream).
