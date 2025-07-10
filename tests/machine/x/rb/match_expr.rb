x = 2
label = (begin
	_t0 = x
	case
	when _t0 == 1
		"one"
	when _t0 == 2
		"two"
	when _t0 == 3
		"three"
	else
		"unknown"
	end
end)
puts(label)
