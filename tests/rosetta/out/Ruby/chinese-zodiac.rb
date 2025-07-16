Info = Struct.new(:animal, :yinYang, :element, :stemBranch, :cycle, keyword_init: true)

def cz(yr, animal, yinYang, element, sc, bc)
	y = (yr - 4)
	stem = (y % 10)
	branch = (y % 12)
	sb = (sc[stem] + bc[branch])
	return Info.new(animal: animal[branch], yinYang: yinYang[(stem % 2)], element: element[((stem / 2))], stemBranch: sb, cycle: ((y % 60) + 1))
end

$animal = ["Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"]
$yinYang = ["Yang", "Yin"]
$element = ["Wood", "Fire", "Earth", "Metal", "Water"]
$stemChArr = ["甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"]
$branchChArr = ["子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"]
[1935, 1938, 1968, 1972, 1976].each do |yr|
	r = cz(yr, $animal, $yinYang, $element, $stemChArr, $branchChArr)
	puts((((((((((((yr).to_s + ": ") + r.element) + " ") + r.animal) + ", ") + r.yinYang) + ", Cycle year ") + (r.cycle).to_s) + " ") + r.stemBranch))
end
