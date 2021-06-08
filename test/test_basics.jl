using Test, Sockets, Erjulix, ErlangTerm

function getPort(start::Integer)
    s = UDPSocket()
    port = start
    while true
        bind(s, Sockets.localhost, port) && break
        port += 1
        port > 65535 && throw(SystemError("no port available"))
    end
    close(s)
    port
end

localhost = Sockets.localhost
pport = getPort(100)
println("pServer setup")
ps = pServer(pport)
erl = UDPSocket()     # erlang test socket

@test ps.state == :runnable
send(erl, localhost, pport, serialize(:srv))
hp, pkg = recvfrom(erl)
msg = deserialize(pkg)
eport = hp.port
@test hp.host == localhost
@test first(msg) == :ok
@test last(msg) == repr(Erjulix._ESM[end])

# test eval 
println("test :eval")
send(erl, localhost, eport, serialize((:eval, "sum(1:10)")))
msg = deserialize(recv(erl))
@test msg == (:ok, 55)

# test call
println("test :call")
send(erl, localhost, eport, serialize((:call, :sum, [collect(11:20)])))
msg = deserialize(recv(erl))
@test msg == (:ok, 155)

# test set
println("test :set")
send(erl, localhost, eport, serialize((:set, :a, collect(21:30))))
msg = deserialize(recv(erl))
@test msg == (:ok, [])
@test Erjulix._ESM[end].a == collect(21:30)

# test exit
println("test :exit")
send(erl, localhost, eport, serialize(:exit))
sleep(0.5)
@test Erjulix._ESM[end]._eServer.state == :done

send(erl, localhost, pport, serialize(:exit))
sleep(0.5)
@test ps.state == :done
