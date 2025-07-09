require 'ostruct'

$data = [OpenStruct.new(a: 1, b: 2), OpenStruct.new(a: 1, b: 1), OpenStruct.new(a: 0, b: 5)]
$sorted = ((($data)).sort_by { |x| [x.a, x.b] }).map { |x| x }
puts($sorted)
