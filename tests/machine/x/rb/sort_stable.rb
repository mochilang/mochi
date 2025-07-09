require 'ostruct'

$items = [OpenStruct.new(n: 1, v: "a"), OpenStruct.new(n: 1, v: "b"), OpenStruct.new(n: 2, v: "c")]
$result = ((($items)).sort_by { |i| i.n }).map { |i| i.v }
puts($result)
