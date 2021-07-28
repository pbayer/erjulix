using Test, Sockets, Erjulix, ErlangTerm

localhost = Sockets.localhost
erl = UDPSocket()     # erlang test socket
ep = Erjulix.getPort(1000)
@test bind(erl, localhost, ep)

pport = Erjulix.getPort(1000)
println("pServer setup")
ps = pServer(pport)

@test ps.state == :runnable
send(erl, localhost, pport, serialize(:srv))
hp, pkg = recvfrom(erl)
msg = deserialize(pkg)
@test hp.host == localhost
@test first(msg) == :ok
@test last(msg) == repr(Erjulix._ESM[end])
println("EvalServer at $(hp.host):$(hp.port)")

# test eval 
println("test :eval")
send(erl, hp.host, hp.port, serialize((:eval, "sum(1:10)")))
println("msg sent!")
sleep(0.5)
@test Erjulix._ESM[end]._eServer.state == :runnable
println("server ok")
pkg = recv(erl)
println("got message from server")
msg = deserialize(pkg)
@test msg == (:ok, 55)

# test call
println("test :call")
send(erl, hp.host, hp.port, serialize((:call, :sum, [collect(11:20)])))
sleep(0.2)
@test Erjulix._ESM[end]._eServer.state == :runnable
pkg = recv(erl)
msg = deserialize(pkg)
@test msg == (:ok, 155)

# test set
println("test :set")
send(erl, hp.host, hp.port, serialize((:set, :a, collect(21:30))))
sleep(0.2)
@test Erjulix._ESM[end]._eServer.state == :runnable
pkg = recv(erl)
msg = deserialize(pkg)
@test msg == (:ok, :nil)
@test Erjulix._ESM[end].a == collect(21:30)

# test exit
println("test :exit")
send(erl, hp.host, hp.port, serialize(:exit))
sleep(0.5)
@test Erjulix._ESM[end]._eServer.state == :done

send(erl, localhost, pport, serialize(:exit))
sleep(0.5)
@test ps.state == :done
