def lcp(strs)
  prefix = strs[0]
  prefix = prefix[0...-1] until strs.all? { |s| s.start_with?(prefix) }
  prefix
end

tokens = STDIN.read.split
exit if tokens.empty?
idx = 0
t = tokens[idx].to_i
idx += 1
out = []
t.times do
  n = tokens[idx].to_i
  idx += 1
  strs = tokens[idx, n]
  idx += n
  out << "\"#{lcp(strs)}\""
end
STDOUT.write(out.join("\n"))
