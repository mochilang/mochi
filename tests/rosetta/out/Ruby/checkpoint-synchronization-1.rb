$partList = ["A", "B", "C", "D"]
$nAssemblies = 3
(1...(($nAssemblies + 1))).each do |cycle|
	puts(("begin assembly cycle " + (cycle).to_s))
	$partList.each do |p|
		puts((p + " worker begins part"))
	end
	$partList.each do |p|
		puts((p + " worker completes part"))
	end
	puts((("assemble.  cycle " + (cycle).to_s) + " complete"))
end
