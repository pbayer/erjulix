defmodule ErjulixTest do
  use ExUnit.Case
  doctest Erjulix

  test "greets the world" do
    assert Erjulix.hello() == :world
  end
end
