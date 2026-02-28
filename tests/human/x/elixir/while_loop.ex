def loop(i) do
  if i < 3 do
    IO.inspect(i)
    loop(i + 1)
  else
    :ok
  end
end

loop(0)
