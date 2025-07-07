map = %{a: 1, b: 2}
json = Jason.encode!(map)
IO.puts(json)
