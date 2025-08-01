# Generated by Mochi compiler v0.10.28 on 2025-07-18T07:03:33Z
defmodule Main do
  def main do
    # products :: list(map())
    products = [
      %{name: "Laptop", price: 1500},
      %{name: "Smartphone", price: 900},
      %{name: "Tablet", price: 600},
      %{name: "Monitor", price: 300},
      %{name: "Keyboard", price: 100},
      %{name: "Mouse", price: 50},
      %{name: "Headphones", price: 200}
    ]

    # expensive :: list(map())
    expensive =
      for p <- Enum.take(Enum.drop(Enum.sort_by(products, fn p -> -p.price end), 1), 3), do: p

    IO.puts("--- Top products (excluding most expensive) ---")

    for item <- expensive do
      IO.puts(Enum.join(Enum.map([item.name, "costs $", item.price], &inspect(&1)), " "))
    end
  end
end

Main.main()
