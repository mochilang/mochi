data = %{"outer" => %{"inner" => 1}}
data = put_in(data["outer"]["inner"], 2)
IO.inspect(get_in(data, ["outer", "inner"]))
