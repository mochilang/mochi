# Generated by Mochi transpiler on 2025-07-21 19:10 +0700
m = %{"a" => 1, "b" => 2, "c" => 3}
IO.inspect(Enum.join(Enum.sort(Map.values(m)), " "))
