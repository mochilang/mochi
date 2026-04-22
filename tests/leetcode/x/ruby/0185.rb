toks = STDIN.read.split
exit if toks.empty?
idx = 0
t = toks[idx].to_i
idx += 1
cases = []
t.times do
  d = toks[idx].to_i
  e = toks[idx + 1].to_i
  idx += 2
  dept_name = {}
  d.times do
    dept_name[toks[idx].to_i] = toks[idx + 1]
    idx += 2
  end
  groups = Hash.new { |h, k| h[k] = [] }
  e.times do
    idx += 1
    name = toks[idx]
    salary = toks[idx + 1].to_i
    dept_id = toks[idx + 2].to_i
    idx += 3
    groups[dept_id] << [name, salary]
  end
  rows = []
  groups.each do |dept_id, items|
    keep = items.map { |name, salary| salary }.uniq.sort.reverse.take(3)
    items.each { |name, salary| rows << [dept_name[dept_id], -salary, name, salary] if keep.include?(salary) }
  end
  rows.sort!
  lines = [rows.length.to_s]
  rows.each { |dept, neg, name, salary| lines << "#{dept},#{name},#{salary}" }
  cases << lines.join("\n")
end
print cases.join("\n\n")
