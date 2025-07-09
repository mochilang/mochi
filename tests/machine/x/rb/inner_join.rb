require 'ostruct'

$customers = [OpenStruct.new(id: 1, name: "Alice"), OpenStruct.new(id: 2, name: "Bob"), OpenStruct.new(id: 3, name: "Charlie")]
$orders = [OpenStruct.new(id: 100, customerId: 1, total: 250), OpenStruct.new(id: 101, customerId: 2, total: 125), OpenStruct.new(id: 102, customerId: 1, total: 300), OpenStruct.new(id: 103, customerId: 4, total: 80)]
$result = (begin
	_res = []
	for o in $orders
		for c in $customers
			if (o.customerId == c.id)
				_res << OpenStruct.new(orderId: o.id, customerName: c.name, total: o.total)
			end
		end
	end
	_res
end)
puts("--- Orders with customer info ---")
for entry in $result
	puts(["Order", entry.orderId, "by", entry.customerName, "- $", entry.total].join(" "))
end
