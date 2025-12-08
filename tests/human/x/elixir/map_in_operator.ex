m = %{1 => "a", 2 => "b"}
IO.inspect(Map.has_key?(m, 1))
IO.inspect(Map.has_key?(m, 3))
