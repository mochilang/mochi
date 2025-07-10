def _sum(v)
  list = nil
  if defined?(MGroup) && v.is_a?(MGroup)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return 0 if !list || list.empty?
  s = 0.0
  list.each { |n| s += n.to_f }
  s
end

nums = [1, 2, 3]
result = (((nums)).select { |n| (n > 1) }).map { |n| _sum(n) }
puts(result)
