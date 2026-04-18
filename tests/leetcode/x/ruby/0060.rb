def get_permutation(n, k)
  digits = (1..n).map(&:to_s)
  fact = Array.new(n + 1, 1)
  (1..n).each { |i| fact[i] = fact[i - 1] * i }
  k -= 1
  out = +''
  n.downto(1) do |rem|
    block = fact[rem - 1]
    idx = k / block
    k %= block
    out << digits.delete_at(idx)
  end
  out
end

lines = STDIN.read.lines.map(&:chomp)
exit if lines.empty?
idx = 0
t = lines[idx].to_i
idx += 1
out = []
t.times do
  n = lines[idx].to_i
  idx += 1
  k = lines[idx].to_i
  idx += 1
  out << get_permutation(n, k)
end
STDOUT.write(out.join("\n"))
