# Requires :yamerl application (comes with Erlang)
:application.ensure_all_started(:yamerl)
people = :yamerl_constr.file('tests/interpreter/valid/people.yaml')
|> Enum.map(fn [map] -> Enum.into(map, %{}) end)

adults =
  people
  |> Enum.filter(fn p -> p["age"] >= 18 end)
  |> Enum.map(fn p -> %{name: p["name"], email: p["email"]} end)

Enum.each(adults, fn a ->
  IO.puts("#{a.name} #{a.email}")
end)
