# Generated by Mochi transpiler v0.10.34 on 2025-07-21 23:34 +0700
ItemsItem = Struct.new(:cat, :val, :flag, keyword_init: true)
items = [ItemsItem.new(cat: "a", val: 10, flag: true), ItemsItem.new(cat: "a", val: 5, flag: false), ItemsItem.new(cat: "b", val: 20, flag: true)]
Result = Struct.new(:cat, :share, keyword_init: true)
result = (begin
  groups = {}
  items.each do |i|
    k = i["cat"]
    groups[k] ||= []
    groups[k] << i
  end
    result = []
  groups.each do |k, items|
    g = { "key" => k, "items" => items }
    result << Result.new(cat: g["key"], share: (begin
      _res = []
      g["items"].each do |x|
        _res << (x["flag"] ? x["val"] : 0)
      end
    _res
  end).sum / (begin
    _res = []
    g["items"].each do |x|
      _res << x["val"]
    end
  _res
end).sum)
end
result
end)
puts(("[" + (result).map{ |x| if x.is_a?(String) then '\'' + x + '\'' elsif x.respond_to?(:to_h) then '{' + x.to_h.map{ |k,v| "'#{k}': #{v.is_a?(String) ? '\'' + v + '\'' : v.to_s}" }.join(', ') + '}' else x.to_s end }.join(', ') + "]"))
