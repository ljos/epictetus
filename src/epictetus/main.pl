:- module(main, [main/0]).
:- use_module(library(dcg/basics)).
:- use_module(sandbox).
:- consult(config).

write_variables_to(_, [error(syntax_error)]).
write_variables_to(_, [error(existence_error)]).
write_variables_to(_, [error(not_at_end_of_stream)]).
write_variables_to(_, [error(time_limit_exceeded)]).
write_variables_to(Channel, [error(instantiation_error,_)]) :-
    maybe(0.01),
    format('PRIVMSG ~s :~s~n',
           [Channel, "... 1,000,000 ............ 10,000,000 years later"]),
    format('PRIVMSG ~s :~s~n',
           [Channel, "      >> 42 << (last release gives the question)"]).
write_variables_to(_, [error(_,_)]).
write_variables_to(Channel, []) :-
    format('PRIVMSG ~s :Yes.~n', [Channel]).
write_variables_to(Channel, [Var]) :-
    format('PRIVMSG ~s :~w~n', [Channel, Var]).
write_variables_to(Channel, [Var|Vars]) :-
    format('PRIVMSG ~s :~w,~n', [Channel, Var]),
    write_variables_to(Channel, Vars).

command(message(_, Channel, Command)) :-
    parse_message(Command, Response),
    format('PRIVMSG ~s, :~s~n', Channel, Response).
command(message(_, Channel, Command)) :-
    evaluate(Command, Vars),
    write_variables_to(Channel, Vars).
command(message(_, Channel, _)) :-
    %% if evaluate fails we should return No.
    %% does not fail on syntax_error or timeout,
    %% those are handled by write_variables_to.
    format('PRIVMSG ~s :No.~n', [Channel]).

ping -->
    "PING :", nonblanks(Value),
    {
     format('PONG :~s~n', [Value]),
     flush_output
    }.

message(message(From, To, Message)) -->
    ":", string(Sender), "!", nonblanks(_), " PRIVMSG ", string(Reciever), " :",
    string(Message),
    {
     atom_codes(From, Sender),
     atom_codes(To, Reciever)
    }.

respond(Request) :-
    phrase(ping, Request).
respond(Request) :-
    phrase(message(Message), Request),
    command(Message).

respond :-
    read_line_to_codes(current_input, Codes),
    ignore(respond(Codes)),
    flush_output,
    respond.

join_channel(Channel) :-
    format('JOIN ~s~n', [Channel]).

connection --> string(_), "+ix", string(_).

wait_for_connection :-
    read_line_to_codes(current_input, Codes),
    (phrase(connection, Codes) ;
     phrase(ping, Codes),
     wait_for_connection ;
     \+ at_end_of_stream,
     wait_for_connection).

connect(Nick, Host:Port, Channels) :-
    tcp_socket(Socket),
    tcp_connect(Socket, Host:Port),
    tcp_open_socket(Socket, Pair),
    set_input(Pair),
    set_output(Pair),
    format('NICK ~s~n', [Nick]),
    flush_output,
    format('USER ~s 0 * :~s~n', [Nick, Nick]),
    flush_output,
    wait_for_connection,
    maplist(join_channel, Channels),
    flush_output,!,
    catch(respond,
          Error,
          (format('Connection lost:~n'),
           print_message(error, Error),
           ignore(close(Pair)))).


start_thread(server(Nick, Host:Port, Channels)) :-
    format(atom(Server), '~s:~w', [Host, Port]),
    thread_create(connect(Nick, Host:Port, Channels),
                  _,
                  [alias(Server),
                   detached(true)]).

main :-
    setof(server(Nick, Host:Port, Channels),
          server(Nick, Host:Port, Channels),
          Servers),
    maplist(start_thread, Servers).
