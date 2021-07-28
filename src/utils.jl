using Random

const chars = ['a':'z'; 'A':'Z'; '0':'9'; ['/','+','-','=']]

"Return a Base64 encoded random passwort of length `len`."
genpasswd(len) = randstring(chars, len)

"Return an available port number ≥ `start`."
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

"Return the `Sockets.InetAddr` of a socket `sock`."
function getHostPort(sock::UDPSocket)
    s = UDPSocket()
    p = getPort(1000)
    @assert bind(s, Sockets.localhost, p) "No port available"
    send(sock, Sockets.localhost, p, serialize(0))
    hp, _ = recvfrom(s)
    hp
end
