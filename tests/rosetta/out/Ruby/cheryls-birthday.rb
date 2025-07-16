Birthday = Struct.new(:month, :day, keyword_init: true)

def monthUnique(b, list)
	c = 0
	list.each do |x|
		if (x.month == b.month)
			c = (c + 1)
		end
	end
	return (c == 1)
end

def dayUnique(b, list)
	c = 0
	list.each do |x|
		if (x.day == b.day)
			c = (c + 1)
		end
	end
	return (c == 1)
end

def monthWithUniqueDay(b, list)
	list.each do |x|
		if ((x.month == b.month) && dayUnique(x, list))
			return true
		end
	end
	return false
end

def bstr(b)
	months = ["", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	return ((months[b.month] + " ") + (b.day).to_s)
end

$choices = [Birthday.new(month: 5, day: 15), Birthday.new(month: 5, day: 16), Birthday.new(month: 5, day: 19), Birthday.new(month: 6, day: 17), Birthday.new(month: 6, day: 18), Birthday.new(month: 7, day: 14), Birthday.new(month: 7, day: 16), Birthday.new(month: 8, day: 14), Birthday.new(month: 8, day: 15), Birthday.new(month: 8, day: 17)]
$filtered = []
$choices.each do |bd|
	if (!monthUnique(bd, $choices))
		$filtered = ($filtered + [bd])
	end
end
$filtered2 = []
$filtered.each do |bd|
	if (!monthWithUniqueDay(bd, $filtered))
		$filtered2 = ($filtered2 + [bd])
	end
end
$filtered3 = []
$filtered2.each do |bd|
	if dayUnique(bd, $filtered2)
		$filtered3 = ($filtered3 + [bd])
	end
end
$filtered4 = []
$filtered3.each do |bd|
	if monthUnique(bd, $filtered3)
		$filtered4 = ($filtered4 + [bd])
	end
end
if (($filtered4).length == 1)
	puts(("Cheryl's birthday is " + bstr($filtered4[0])))
else
	puts("Something went wrong!")
end
