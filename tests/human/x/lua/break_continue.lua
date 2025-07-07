local numbers = {1,2,3,4,5,6,7,8,9}
for _, n in ipairs(numbers) do
  if n % 2 == 0 then
    -- continue
  elseif n > 7 then
    break
  else
    print("odd number: " .. n)
  end
end
