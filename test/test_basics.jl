using Test, Sockets, Erjulix, ErlangTerm

function getPort(start::Integer)
    s = UDPSocket()
    sock = start
    while true
        bind(s, Sockets.localhost, sock) && break
        sock += 1
    end
    close(s)
    sock
end

localhost = Sockets.localhost
pport = getPort(100)
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
send(erl, localhost, eport, serialize((:eval, "sum(1:10)")))
msg = deserialize(recv(erl))
@test msg == (:ok, 55)

# test call
send(erl, localhost, eport, serialize((:call, :sum, [collect(11:20)])))
msg = deserialize(recv(erl))
@test msg == (:ok, 155)

# test set
send(erl, localhost, eport, serialize((:set, :a, collect(21:30))))
msg = deserialize(recv(erl))
@test msg == (:ok, [])
@test Erjulix._ESM[end].a == collect(21:30)

# test exit
send(erl, localhost, eport, serialize(:exit))
sleep(0.5)
@test Erjulix._ESM[end]._eServer.state == :done

send(erl, localhost, pport, serialize(:exit))
sleep(0.5)
@test ps.state == :done
