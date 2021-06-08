# Erjulix

Connecting Erlang, Julia, Elixir

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://pbayer.github.io/erjulix/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://pbayer.github.io/erjulix/dev)
[![Build Status](https://github.com/pbayer/erjulix/workflows/CI/badge.svg)](https://github.com/pbayer/erjulix/actions)
[![Coverage](https://codecov.io/gh/pbayer/erjulix/branch/master/graph/badge.svg)](https://codecov.io/gh/pbayer/erjulix)

## Project

This is my ambitious little project to connect the different worlds of Erlang/Elixir and Julia:

- Provide one package for three platforms/languages,
- Allow them to talk to and call each other.

Now Erlang and Elixir processes can send messages to each other since they run on the same BEAM platform and share PIDs. But how about sending messages to Julia and back to Erlang/Elixir.

## A sample session

In the Julia REPL we start a pServer task, which on demand spawns an `EvalServer` task with its own module namespace.

```julia
julia> using Erjulix, Sockets

julia> p = pServer(getipaddr(), 6000) # start a pServer on UDP socket 6000
Task (runnable) @0x0000000117731550
```

In the Elixir REPL we request a Julia EvalServer and use it to 
eval Julia expressions or to call Julia functions.

```elixir
iex(1)> {:ok, jl, _} = :ejx_udp.srv({{192,168,2,113}, 6000}) # get an eval server from Julia
{:ok, {{192, 168, 2, 113}, 58365}, {:ok, "Main.##esm#257"}}
iex(2)> :ejx_udp.eval(jl, "using .Threads")
{:ok, []}
iex(3)> :ejx_udp.call(jl, :threadid)
{:ok, 2}
iex(4)> :ejx_udp.call(jl, :factorial, [20])
{:ok, 2432902008176640000}
iex(5)> :ejx_udp.call(jl, :factorial, [50])
{:error,
 "OverflowError(\"50 is too large to look up in the table; consider using `factorial(big(50))` instead\")"}
iex(6)> :ejx_udp.eval(jl, """    # define a function on the Julia server
...(6)> function fact(x)
...(6)>     factorial(big(x))
...(6)> end
...(6)> """)
{:ok, "Main.##esm#257.fact"}
iex(7)> :ejx_udp.call(jl, :fact, [50])
{:ok, 30414093201713378043612608166064768844377641568960512000000000000}
iex(8)> :timer.tc(:ejx_udp, :call, [jl, :fact, [55]])
{528,
 {:ok,
  12696403353658275925965100847566516959580321051449436762275840000000000000}}
```

The last timing shows that the ping-pong for calling the created Julia fact function with data from Elixir and getting the result back takes roughly 500 µs with both sessions running on the same machine (MacBook Pro).

```elixir
iex(9)> a = Enum.map(1..10, fn _ -> :rand.uniform() end)
[0.9414436609049482, 0.08244595999142224, 0.6727398779368937,
 0.18612089183158875, 0.7414592106015152, 0.7340558985797445,
 0.9511971092470349, 0.7139960750204088, 0.31514816254491884, 0.94168140313657]
iex(10)> :ejx_udp.set(jl, :a, a)  # create variable a on the Julia server
{:ok, []}
```

Back in the Julia REPL:

```julia
julia> exmod = Erjulix._ESM[1]  # get access to the server module
Main.##esm#257

julia> exmod.a                  # and to the created variable a
10-element Vector{Any}:
 0.9414436609049482
 0.08244595999142224
 0.6727398779368937
 0.18612089183158875
 ⋮
 0.9511971092470349
 0.7139960750204088
 0.31514816254491884
 0.94168140313657

julia> using Plots ....
```

**Caveat:** of course accessing the server module that way is not thread-safe and thus should not be done concurrently.

## ToDo

Implement an Elixir server to serve Julia with Elixir/Erlang functionality.

## Rationale

This is a quick prototype for interoperability based on [Erlang`s Term Format](http://erlang.org/doc/apps/erts/erl_ext_dist.html) over UDP. It allows applications in Web services, IoT or microservices. A more general application should be done with [OSC](http://opensoundcontrol.org).

## Installation

When available in the Julia registry, you can install the package with

```julia
pkg> add Erjulix
```

If [available in Hex](https://hex.pm/docs/publish), the package can be installed in Elixir by adding `erjulix` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:erjulix, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/erjulix](https://hexdocs.pm/erjulix).

