numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9};
for _, n in ipairs(numbers) do
  if ((n % 2) == 0) then
    goto __cont_1;
  end;
  if (n > 7) then
    break;
  end;
  print(string.format("odd number: %s", n));
  ::__cont_1::;
end;
