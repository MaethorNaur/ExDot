defmodule ExDotTest do
  use ExUnit.Case
  doctest ExDot

  test "greets the world" do
    assert ExDot.hello() == :world
  end
end
