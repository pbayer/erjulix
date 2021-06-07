# Erjulix

Connecting Erlang, Julia, Elixir

## Project

This is my little ambitious project to connect the different worlds of Erlang/Elixir and Julia:

- Provide one package for three platforms/languages,
- Allow them to talk to and to call each other.

Now Erlang and Elixir processes can send messages to each other since they run on the same BEAM platform and share PIDs. But how about sending messages to Julia and back to Erlang/Elixir.

## A sample session

In the Julia REPL we start a pServer task, which spawns an
EvalServer task with its own module namespace on demand.

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
iex(7)> :ejx_udp.call(jl, :fact, [500])   # call it
{:ok,
 1220136825991110068701238785423046926253574342803192842192413588385845373153881997605496447502203281863013616477148203584163378722078177200480785205159329285477907571939330603772960859086270429174547882424912726344305670173270769461062802310452644218878789465754777149863494367781037644274033827365397471386477878495438489595537537990423241061271326984327745715546309977202781014561081188373709531016356324432987029563896628911658974769572087926928871281780070265174507768410719624390394322536422605234945850129918571501248706961568141625359056693423813008856249246891564126775654481886506593847951775360894005745238940335798476363944905313062323749066445048824665075946735862074637925184200459369692981022263971952597190945217823331756934581508552332820762820023402626907898342451712006207714640979456116127629145951237229913340169552363850942885592018727433795173014586357570828355780158735432768888680120399882384702151467605445407663535984174430480128938313896881639487469658817504506926365338175055478128640000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000}
```

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

