# Generated by Mochi compiler v0.10.30 on 2025-07-19T00:24:42Z
$door = 1
$incrementer = 0
(1...101).each do |current|
	line = (("Door " + (current).to_s) + " ")
	if (current == $door)
		line = (line + "Open")
		$incrementer = ($incrementer + 1)
		$door = (($door + (2 * $incrementer)) + 1)
	else
		line = (line + "Closed")
	end
	puts(line)
end
