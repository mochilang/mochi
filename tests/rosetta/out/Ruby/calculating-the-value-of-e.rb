def _sliceString(s, i, j)
  start = i
  finish = j
  chars = s.chars
  n = chars.length
  start += n if start < 0
  finish += n if finish < 0
  start = 0 if start < 0
  finish = n if finish > n
  finish = start if finish < start
  chars[start...finish].join
end

def absf(x)
	if (x < 0.0)
		return (-x)
	end
	return x
end

def pow10(n)
	r = 1.0
	i = 0
	while (i < n)
		r = (r * 10.0)
		i = (i + 1)
	end
	return r
end

def formatFloat(f, prec)
	scale = pow10(prec)
	scaled = (((f * scale)) + 0.5)
	n = (scaled)
	digits = (n).to_s
	while ((digits).length <= prec)
		digits = ("0" + digits)
	end
	intPart = _sliceString(digits, 0, ((digits).length - prec))
	fracPart = _sliceString(digits, ((digits).length - prec), (digits).length)
	return ((intPart + ".") + fracPart)
end

$epsilon = 0.000000000000001
$factval = 1
$e = 2.0
$n = 2
$term = 1.0
while true
	$factval = ($factval * $n)
	$n = ($n + 1)
	$term = (1.0 / ($factval))
	$e = ($e + $term)
	if (absf($term) < $epsilon)
		break
	end
end
puts(("e = " + formatFloat($e, 15)))
