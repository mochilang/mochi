def snap(seen)
  vals = seen.keys.sort
  parts = []
  st = prev = nil
  vals.each do |v|
    if st.nil?
      st = prev = v
    elsif v == prev + 1
      prev = v
    else
      parts << "[#{st},#{prev}]"
      st = prev = v
    end
  end
  parts << "[#{st},#{prev}]" unless st.nil?
  '[' + parts.join(',') + ']'
end
d = STDIN.read.split
if d.length > 0
  idx = 0; t = d[idx].to_i; idx += 1; cases = []
  t.times do
    ops = d[idx].to_i; idx += 1; seen = {}; snaps = []
    ops.times do
      op = d[idx]; idx += 1
      if op == 'A' then seen[d[idx].to_i] = true; idx += 1 else snaps << snap(seen) end
    end
    cases << '[' + snaps.join(',') + ']'
  end
  print cases.join("\n\n")
end
