data = {"outer" => {"inner" => 1}}
data["outer"]["inner"] = 2
puts([data["outer"]["inner"]].join(" "))
