nums = [1, 2, 3]
letters = ['A', 'B']
pairs = []
nums.each do |n|
  next unless n.even?
  letters.each do |l|
    pairs << {n: n, l: l}
  end
end
puts '--- Even pairs ---'
pairs.each { |p| puts "#{p[:n]} #{p[:l]}" }
