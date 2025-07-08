products = [
  %{name: "Laptop", price: 1500},
  %{name: "Smartphone", price: 900},
  %{name: "Tablet", price: 600},
  %{name: "Monitor", price: 300},
  %{name: "Keyboard", price: 100},
  %{name: "Mouse", price: 50},
  %{name: "Headphones", price: 200}
]

expensive =
  products
  |> Enum.sort_by(& &1.price, :desc)
  |> Enum.drop(1)
  |> Enum.take(3)

IO.puts("--- Top products (excluding most expensive) ---")
Enum.each(expensive, fn item ->
  IO.puts("#{item.name} costs $#{item.price}")
end)
