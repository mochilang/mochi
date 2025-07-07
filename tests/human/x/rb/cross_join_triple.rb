nums = [1, 2]
letters = ['A', 'B']
bools = [true, false]
combos = []
nums.each do |n|
  letters.each do |l|
    bools.each do |b|
      combos << {n: n, l: l, b: b}
    end
  end
end
puts '--- Cross Join of three lists ---'
combos.each { |c| puts "#{c[:n]} #{c[:l]} #{c[:b]}" }
