# Generated by Mochi transpiler on 2025-07-21 19:10 +0700
products = [%{name: "Laptop", price: 1500}, %{name: "Smartphone", price: 900}, %{name: "Tablet", price: 600}, %{name: "Monitor", price: 300}, %{name: "Keyboard", price: 100}, %{name: "Mouse", price: 50}, %{name: "Headphones", price: 200}]
expensive = Enum.map(Enum.take(Enum.drop(Enum.sort_by(for p <- products do
  %{k: -p.price, v: p}
end, fn (x) -> x.k end), 1), 3), fn (x) -> x.v end)
IO.puts("--- Top products (excluding most expensive) ---")
Enum.each(expensive, fn (item) -> IO.puts("#{Kernel.inspect(item.name)} costs $ #{Kernel.inspect(item.price)}") end)
