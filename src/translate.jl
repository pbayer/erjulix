using ErlangTerm
import JSON, JSONWebTokens

"Serialize `data`, sha-256 encoded with `key` if it is not empty."
function serializek(data, key::AbstractString)
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

"Deserialize a `binary`, sha-256 decoded with `key` if it is not empty."
function deserializek(binary::Vector{UInt8}, key::AbstractString)
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
