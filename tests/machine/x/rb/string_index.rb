def _indexString(s, i)
  idx = i
  chars = s.chars
  idx += chars.length if idx < 0
  raise 'index out of range' if idx < 0 || idx >= chars.length
  chars[idx]
end

s = "mochi"
puts(_indexString(s, 1))
