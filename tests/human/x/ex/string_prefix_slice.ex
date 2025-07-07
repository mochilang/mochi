prefix = "fore"
s1 = "forest"
IO.inspect(String.slice(s1, 0, String.length(prefix)) == prefix)
s2 = "desert"
IO.inspect(String.slice(s2, 0, String.length(prefix)) == prefix)
