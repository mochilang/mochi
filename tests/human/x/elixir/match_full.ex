x = 2
label =
  case x do
    1 -> "one"
    2 -> "two"
    3 -> "three"
    _ -> "unknown"
  end
IO.puts(label)

day = "sun"
mood =
  case day do
    "mon" -> "tired"
    "fri" -> "excited"
    "sun" -> "relaxed"
    _ -> "normal"
  end
IO.puts(mood)

ok = true
status = if ok, do: "confirmed", else: "denied"
IO.puts(status)

defmodule Util do
  def classify(0), do: "zero"
  def classify(1), do: "one"
  def classify(_), do: "many"
end

IO.puts(Util.classify(0))
IO.puts(Util.classify(5))
