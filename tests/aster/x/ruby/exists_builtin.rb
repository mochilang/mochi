# Generated by Mochi transpiler v0.10.33 on 2025-07-21 16:57 +0700
data = [1, 2]
flag = (!(begin
  _res = []
  data.each do |x|
    if x == 1
      _res << x
    end
  end
  _res
end).empty?()).to_s()
puts(flag)
