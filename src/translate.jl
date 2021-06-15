using ErlangTerm
import JSON, JSONWebTokens

function serializek(data, key)
    if isempty(key)
        serialize(data)
    else
        JSONWebTokens.encode(
            JSONWebTokens.HS256(key),
            data
            |> serialize
            |> JSON.json
        ) |> Vector{UInt8}
    end
end

function deserializek(binary::Vector{UInt8}, key)
    if isempty(key)
        deserialize(binary)
    else
        j = JSONWebTokens.decode(
            JSONWebTokens.HS256(key),
            String(binary) 
        )
        deserialize(convert(Vector{UInt8}, j))
    end
end
