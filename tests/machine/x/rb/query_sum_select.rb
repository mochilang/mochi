# Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
$nums = [1, 2, 3]
$result = (((($nums)).select { |n| (n > 1) }).map { |n| n }).sum(0.0)
puts($result)
