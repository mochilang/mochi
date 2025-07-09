def classify(n)
	return (begin
	_t0 = n
	case
	when _t0 == 0
		"zero"
	when _t0 == 1
		"one"
	else
		"many"
	end
end)
end

$x = 2
$label = (begin
	_t1 = $x
	case
	when _t1 == 1
		"one"
	when _t1 == 2
		"two"
	when _t1 == 3
		"three"
	else
		"unknown"
	end
end)
puts($label)
$day = "sun"
$mood = (begin
	_t2 = $day
	case
	when _t2 == "mon"
		"tired"
	when _t2 == "fri"
		"excited"
	when _t2 == "sun"
		"relaxed"
	else
		"normal"
	end
end)
puts($mood)
$ok = true
$status = (begin
	_t3 = $ok
	case
	when _t3 == true
		"confirmed"
	when _t3 == false
		"denied"
	else
		nil
	end
end)
puts($status)
puts(classify(0))
puts(classify(5))
