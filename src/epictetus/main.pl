:- module(main, []).
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

command(message(Channel, _, Command)) :-
    parse_message(Command, Response),
    format('PRIVMSG ~s, :~s~n', Channel, Response).
command(message(Channel, _, Command)) :-
    evaluate(Command, Vars),
    write_variables_to(Channel, Vars).
command(message(Channel, _, _)) :-
    %% if evaluate fails we should return No.
    %% does not fail on syntax_error or timeout,
    %% those are handled by write_variables_to.
    format('PRIVMSG ~s :No.~n', [Channel]).

ping -->
    "PING :", nonblanks(Value),
    {
     format('PONG :~s~n', [Value])
    }.

message(message(Channel, Message)) -->
    "PRIVMSG ", Channel, " :", Message.
message(message(From, To, Message)) -->
    ":", string(From), "!", string(_),
    blank, "PRIVMSG", blank, string(To), blank,
    ":", string(Message).

respond(Request) :-
    phrase(ping(Pong), Request).
respond(Request) :-
    phrase(message(Message), Request),
    command(Message).

respond :-
    read_line_to_codes(current_input, Codes),
    respond(Codes).

join_channel(Channel) :-
    format('JOIN %s~n', [Channel]).

connection --> string(_), "+ix", string(_).

wait_for_connection :-
    read_line_to_codes(current_input, Codes),
    (phrase(connection, Codes) ;
     phrase(ping(Pong), Codes),
     wait_for_connection ;
     wait_for_connection).

connect(Nick, Host:Port, Channels) :-
    tcp_socket(Socket),
    tcp_connect(Socket, Host:Port),
    tcp_open_socket(Socket, Input, Output),
    format('Connected to ~s on port ~s.~n', [Host, Port]),
    set_input(Input),
    set_output(Output),
    format('NICK %s', [Nick]),
    format('USER %s 0 * :%s', [Nick, Nick]),
    wait_for_connection,
    maplist(join_channel, Channels),
    catch(respond,
          Error,
          (format('Connection lost:~n'),
           print_message(error, Error),
           ignore(close(Input)),
           ignore(close(Output)),
           sleep(1),
           format('~nReconnectiong...~n'),
           connect(Nick, Host:Port, Channels))).

start_thread(server(Nick, Host:Port, Channels)) :-
    thread_create(connect,
                  _,
                  [alias(Host:Port),
                   detached(true)]).

main :-
    setof(server(Nick, Host:Port, Channels),
          server(Nick, Host:Port, Channels),
          Servers),
    maplist(start_thread, Servers).
