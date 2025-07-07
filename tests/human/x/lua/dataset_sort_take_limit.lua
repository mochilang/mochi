local products = {
  {name = "Laptop", price = 1500},
  {name = "Smartphone", price = 900},
  {name = "Tablet", price = 600},
  {name = "Monitor", price = 300},
  {name = "Keyboard", price = 100},
  {name = "Mouse", price = 50},
  {name = "Headphones", price = 200}
}
table.sort(products, function(a,b) return a.price > b.price end)
print("--- Top products (excluding most expensive) ---")
local count = 0
for i=2,#products do
  if count >= 3 then break end
  local p = products[i]
  print(p.name .. " costs $ " .. p.price)
  count = count + 1
end
