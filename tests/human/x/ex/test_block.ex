ExUnit.start()

defmodule MyTest do
  use ExUnit.Case, async: true
  test "addition works" do
    x = 1 + 2
    assert x == 3
  end
end

IO.puts("ok")
