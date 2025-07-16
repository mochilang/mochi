def merge(base, update)
	result = {}
	base.each do |k|
		result[k] = base[k]
	end
	update.each do |k|
		result[k] = update[k]
	end
	return result
end

def main()
	base = {"name" => "Rocket Skates", "price" => 12.75, "color" => "yellow"}
	update = {"price" => 15.25, "color" => "red", "year" => 1974}
	result = merge(base, update)
	puts(result)
end

main()
