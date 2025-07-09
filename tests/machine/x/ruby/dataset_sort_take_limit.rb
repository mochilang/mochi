require 'ostruct'

products = [OpenStruct.new(name: "Laptop", price: 1500), OpenStruct.new(name: "Smartphone", price: 900), OpenStruct.new(name: "Tablet", price: 600), OpenStruct.new(name: "Monitor", price: 300), OpenStruct.new(name: "Keyboard", price: 100), OpenStruct.new(name: "Mouse", price: 50), OpenStruct.new(name: "Headphones", price: 200)]
expensive = (((((products)).sort_by { |p| (-p.price) }).drop(1)).take(3)).map { |p| p }
puts(["--- Top products (excluding most expensive) ---"].join(" "))
for item in expensive
	puts([item.name, "costs $", item.price].join(" "))
end
