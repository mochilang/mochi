x = 8
msg = if x > 10 do
  "big"
else
  if x > 5 do
    "medium"
  else
    "small"
  end
end
IO.puts(msg)
