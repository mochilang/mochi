scores = %{"alice" => 1}
# Map.put returns a new map with the key/value added
scores = Map.put(scores, "bob", 2)
IO.inspect(scores["bob"])
