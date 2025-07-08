defmodule Person do
  defstruct [:name, :age, :status]
end

people = [
  %Person{name: "Alice", age: 17, status: "minor"},
  %Person{name: "Bob", age: 25, status: "unknown"},
  %Person{name: "Charlie", age: 18, status: "unknown"},
  %Person{name: "Diana", age: 16, status: "minor"}
]

people = Enum.map(people, fn p ->
  if p.age >= 18 do
    %{p | status: "adult", age: p.age + 1}
  else
    p
  end
end)

ExUnit.start()

defmodule UpdateTest do
  use ExUnit.Case, async: true
  test "update adult status" do
    assert Enum.at(people, 1) == %Person{name: "Bob", age: 26, status: "adult"}
    assert Enum.at(people, 2) == %Person{name: "Charlie", age: 19, status: "adult"}
  end
end

IO.puts("ok")
