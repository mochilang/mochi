def _max(v)
  list = nil
  if v.respond_to?(:Items)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return 0 if !list || list.empty?
  list.max
end
def _min(v)
  list = nil
  if v.respond_to?(:Items)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return 0 if !list || list.empty?
  list.min
end

nums = [3, 1, 4]
puts([_min(nums)].join(" "))
puts([_max(nums)].join(" "))
