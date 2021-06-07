# Erjulix

Connecting Erlang, Julia, Elixir

## Project

This is my little ambitious project to connect the different worlds of Erlang/Elixir and Julia:

- Provide one package for three platforms/languages,
- Allow them to talk to and to call each other.

Now Erlang and Elixir processes can send messages to each other since they run on the same BEAM platform and share PIDs. But how about sending messages to Julia and back to Erlang/Elixir.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `erjulix` to your list of dependencies in `mix.exs`:

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

