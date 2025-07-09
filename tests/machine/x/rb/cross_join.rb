require 'ostruct'

$customers = [OpenStruct.new(id: 1, name: "Alice"), OpenStruct.new(id: 2, name: "Bob"), OpenStruct.new(id: 3, name: "Charlie")]
$orders = [OpenStruct.new(id: 100, customerId: 1, total: 250), OpenStruct.new(id: 101, customerId: 2, total: 125), OpenStruct.new(id: 102, customerId: 1, total: 300)]
$result = (begin
	_res = []
	for o in $orders
		for c in $customers
			_res << OpenStruct.new(orderId: o.id, orderCustomerId: o.customerId, pairedCustomerName: c.name, orderTotal: o.total)
		end
	end
	_res
end)
puts("--- Cross Join: All order-customer pairs ---")
for entry in $result
	puts(["Order", entry.orderId, "(customerId:", entry.orderCustomerId, ", total: $", entry.orderTotal, ") paired with", entry.pairedCustomerName].join(" "))
end
