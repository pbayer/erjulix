
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

function getHostPort(sock::UDPSocket)
    s = UDPSocket()
    p = getPort(1000)
    @assert bind(s, Sockets.localhost, p) "No port available"
    send(sock, Sockets.localhost, p, serialize(0))
    hp, _ = recvfrom(s)
    hp
end
