x = 2
label = case x
when 1 then "one"
when 2 then "two"
when 3 then "three"
else "unknown"
end
puts label

day = "sun"
mood = case day
when "mon" then "tired"
when "fri" then "excited"
when "sun" then "relaxed"
else "normal"
end
puts mood

ok = true
status = case ok
when true then "confirmed"
when false then "denied"
end
puts status

def classify(n)
  case n
  when 0 then "zero"
  when 1 then "one"
  else "many"
  end
end
puts classify(0)
puts classify(5)
