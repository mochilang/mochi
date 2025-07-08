require 'ostruct'

customers = [OpenStruct.new(id: 1, name: "Alice"), OpenStruct.new(id: 2, name: "Bob")]
orders = [OpenStruct.new(id: 100, customerId: 1), OpenStruct.new(id: 101, customerId: 2)]
items = [OpenStruct.new(orderId: 100, sku: "a"), OpenStruct.new(orderId: 101, sku: "b")]
result = (begin
	_res = []
	for o in orders
		for c in customers
			if (o.customerId == c.id)
				for i in items
					if (o.id == i.orderId)
						_res << OpenStruct.new(name: c.name, sku: i.sku)
					end
				end
			end
		end
	end
	_res
end)
puts(["--- Multi Join ---"].join(" "))
for r in result
	puts([r.name, "bought item", r.sku].join(" "))
end
