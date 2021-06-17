-module(ejx_udp).
-export([call/2, call/3, call/4, eval/2, eval/3, set/3, set/4,
    client/2, client/3, srv/1]).

%% @type addr(). Can be 
%% - a port on the localhost or 
%% - a tuple of an IP address and a port number or
%% - a tuple of an IP address, a port number and a key.
-type addr() :: inet:port_number() 
             | {inet:ip_address(), inet:port_number()}
             | {inet:ip_address(), inet:port_number(), string()}.

%% A Julia term is either an atom referring to a variable or 
%% callable or a string containing a Julia expression.
-type jterm() :: atom() | string().

-type atoms() :: atom() | [atom()].

-type vals() :: any() | [] | list().

-type response() :: {ok, any()} | {error, string()}.

%% @doc Send a Julia 'Term' with 'Args' to an EvalServer 
%% at 'Addr' and receive and return the answer. 
%% After 'Timeout' return a 'timeout'.
%% @end
-spec call(addr(), jterm(), vals(), timeout()) -> response().
call(Addr, Term, Args, Timeout) ->
    client(Addr, {call, Term, Args}, Timeout).

%% @doc Send a Julia 'Term' with 'Args' to an EvalServer 
%% at 'Addr' and receive and return the answer. 
%% After 5s return a 'timeout'.
%% @end
-spec call(addr(), jterm(), vals()) -> response().
call(Addr, Term, Args) ->
    client(Addr, {call, Term, Args}, 5000).

%% @doc Send a Julia 'Term' without arguments to an EvalServer 
%% at 'Addr' and receive and return the answer. 
%% After 5s return a 'timeout'.
%% @end
-spec call(addr(), jterm()) -> response().
call(Addr, Term) ->
    client(Addr, {call, Term, []}, 5000).

%% @doc Send a Julia 'Term' for evaluation to an EvalServer 
%% at 'Addr' and receive and return the answer. 
%% After 'Timeout' return a 'timeout'.
%% @end
-spec eval(addr(), jterm(), timeout()) -> response().
eval(Addr, Term, Timeout) ->
    client(Addr, {eval, Term}, Timeout).

%% @doc Send a Julia 'Term' for evaluation to an EvalServer 
%% at 'Addr' and receive and return the answer. 
%% After 5s return a 'timeout'.
%% @end
-spec eval(addr(), jterm()) -> response().
eval(Addr, Term) ->
    client(Addr, {eval, Term}, 5000).

%% @doc Send an atom or a list of atoms to a Julia EvalServer
%% at 'Addr' to set it/them to a value or a list of values and
%% to make it/them available for further computations.
%% @end
-spec set(addr(), atoms(), vals(), timeout()) -> response().
set(Addr, Atoms, Vals, Timeout) ->
    client(Addr, {set, Atoms, Vals}, Timeout). 

-spec set(addr(), atoms(), vals()) -> response().
set(Addr, Atoms, Vals) ->
    client(Addr, {set, Atoms, Vals}, 5000).

client(Addr, Msg) ->
    client(Addr, Msg, 5000).

client(Port, Msg, Timeout) when is_integer(Port) ->
    client({"localhost", Port}, Msg, Timeout);

client({Host, Port}, Msg, Timeout) ->
    client({Host, Port, ""}, Msg, Timeout);

client({Host, Port, Key}, Msg, Timeout) ->
    Bin = term_to_binary_k(Msg, Key),
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Socket, Host, Port, Bin),
    Value = receive
        {udp, Socket, _, _, Recv} ->
            binary_to_term_k(Recv, Key)
        after Timeout ->
            timeout
        end,
    gen_udp:close(Socket),
    Value.

-spec srv(addr()) -> {ok, addr(), any()}.
srv({Host, Port}) ->
    srv({Host, Port, ""});

srv({Host, Port, Key}) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Socket, Host, Port, term_to_binary_k(srv, Key)),
    Value = receive
        {udp, Socket, Shost, Sport, Recv} ->
            Ret = binary_to_term_k(Recv, Key),
            case Ret of
                {ok, NKey, Mod} -> {ok, {Shost, Sport, NKey}, Mod};
                {ok, Mod}       -> {ok, {Shost, Sport}, Mod}
            end
        after 5000 ->
            timeout
        end,
    gen_udp:close(Socket),
    Value;

srv(Port) ->
    srv({"localhost", Port}).

term_to_binary_k(T, "") ->
    term_to_binary(T);

term_to_binary_k(T, Key) ->
    L = binary_to_list(term_to_binary(T)),
    jwerl:sign(L, hs256, Key).

binary_to_term_k(B, "") ->
    binary_to_term(B);

binary_to_term_k(B, Key) ->
    {ok, L} = jwerl:verify(B, hs256, Key),
    binary_to_term(list_to_binary(L)).
