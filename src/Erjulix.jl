module Erjulix

using ErlangTerm, Sockets

export eServer, pServer, EvalServer, recv_erl, send_erl

# Vector of created eServer modules
const _ESM = Module[]

"""
    eServer(port::Integer)

Create a module with an `EvalServer` listening to an UDP `port` 
in its namespace and return the module. 
"""
function eServer(host::IPAddr, port::Integer)
    sock = UDPSocket()
    if bind(sock, host, port)
        mdl = Module(gensym(:esm))
        t = Threads.@spawn EvalServer(sock, mdl)
        Core.eval(mdl, :(_socket = $sock))
        Core.eval(mdl, :(_eServer = $t))
        push!(_ESM, mdl)
        mdl
    else
        println(stderr, "port $port not available")
        close(sock)
    end
end
eServer(port::Integer) = eServer(Sockets.localhost, port)

"""
    EvalServer(port::Integer, mod::Module)

An `EvalServer` runs as a task with its own module namespace.
It receives UDP message tuples from Erlang:

- `(:eval, term)`: where `term` is a `Symbol` or a `String`,
- `(:call, term, args)`: with `term` as above and `args` a `Vector{Any}`,
- `(:set, atoms, vals)`: where `atoms` is a `Symbol` or a vector
    of them and `vals` is a value `Any` or a vector of them.

It then evaluates the messages in its namespace as

1. strings to parse or symbols to evaluate,
2. functions to execute with arguments or 
3. variables to create or to assign values to.
   
It sends a result tuple back to the Erlang client.

The server finishes if it gets an `"exit"` or `:exit` message. 
"""
function EvalServer(sock::UDPSocket, mod::Module)
    while true
        hp, msg = recvfrom(sock)
        isexit(msg) && break
        val = deserialize(msg)
    # println(@show val)
        if val == :exit 
            send(sock, hp.host, hp.port, serialize((:ok, :done)))
            break
        else
            msg = try
                res = _exec(mod, val)
                serialize((:ok, res))
            catch exc
            # rethrow()
                serialize((:error, repr(exc)))
            end
            send(sock, hp.host, hp.port, msg)
        end
    end
    close(sock)
    println("EvalServer $mod done")
end

function _exec(m, val)
    # println("received: $val")
    exec(m, Val(first(val)), val[2:end]...)
end
function exec(m, ::Val{:eval}, str::String)
    Core.eval(m, Meta.parse(str))
end
function exec(m, ::Val{:eval}, sym::Symbol)
    Core.eval(m, sym)
end
function exec(m, ::Val{:call}, sym::Symbol, args)
    Base.invokelatest(Core.eval(m, sym), args...)
end
function exec(m, ::Val{:call}, str::String, args)
    Core.eval(m, Meta.parse(str))(args...)
end
function exec(m, ::Val{:set}, x::Symbol, arg)
    (Core.eval(m, :($x = $arg)); nothing)
end
function exec(m, ::Val{:set}, xs::Vector{Any}, args::Vector{Any})
    for (i, x) in enumerate(xs)
        Core.eval(m, :($x = $(args[i])))
    end
    nothing
end
function exec(_, cmd, args...)
    throw(ArgumentError("cannot $cmd $args"))
end

isexit(msg::Vector{UInt8}) = String(copy(msg)) == "exit" 
isexit(msg) = msg == :exit

"""
```
pServer(port::Integer)
pServer(host::IPAddr, port::Integer)
```

Start a server listening to an UDP `port` and starting 
parallel `EvalServer`s if requested. 
"""
function pServer(host::IPAddr, port::Integer)
    sock = UDPSocket()
    if bind(sock, host, port)
        Threads.@spawn _listen(sock, port)
    else
        println("host $host, port $port not available")
        close(sock)
    end
end
pServer(port::Integer) = pServer(ip"127.0.0.1", port)

# listen to a UDP socket and start a parallel 
# EvalServer if requested.
function _listen(sock, port)
    while true
        hp, msg = recvfrom(sock)
        isexit(msg) && break
        val = deserialize(msg)
        if val == :exit 
            send(sock, hp.host, hp.port, serialize((:ok, :done)))
            break
        elseif val == :srv
            md = parServer(hp)
            println("parServer $md")
        else
            println("parServer cannot $val")
        end
    end
    println("pServer at port $port done")
    close(sock)
end

# Start a parallel EvalServer at a random port and 
# respond over its socket to the requesting client
function parServer(client::Sockets.InetAddr)
    md = client.host == Sockets.localhost ? eServer(0) : eServer(getipaddr(), 0)
    send(md._socket, client.host, client.port, serialize((:ok, md)))
    md
end

function recv_erl(socket::UDPSocket, timeout::Real=5)
    cond = Condition()
    Timer(_ -> notify(cond), timeout)
    t = @async begin
        msg = recv(socket)
        notify(cond)
        msg
    end
    wait(cond)
    t.state == :done ? 
        deserialize(fetch(t)) : 
        :timeout
end

function recv_erl(port::Integer, timeout::Real=5)
    s = UDPSocket()
    rec = if bind(s, ip"127.0.0.1", port)
        recv_erl(s, timeout)
    else
        println(stderr, "port $port is not available")
        nothing
    end
    close(s)
    rec
end

function send_erl(host::IPAddr, port::Integer, msg)
    s = UDPSocket()
    send_erl(s, host, port, msg)
    close(s)
end

function send_erl(s::UDPSocket, host::IPAddr, port::Integer, msg)
    send(s, host, port, serialize(msg))
end

end
