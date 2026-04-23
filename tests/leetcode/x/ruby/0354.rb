def max_env(e)
  e.sort_by! { |w,h| [w, -h] }
  tails = []
  e.each do |_, h|
    lo = 0; hi = tails.length
    while lo < hi
      m = (lo + hi) / 2
      if tails[m] < h then lo = m + 1 else hi = m end
    end
    lo == tails.length ? tails << h : tails[lo] = h
  end
  tails.length
end
d=STDIN.read.split.map(&:to_i)
if d.length>0
 idx=0;t=d[idx];idx+=1;out=[]
 t.times do
  n=d[idx];idx+=1;e=[];n.times{e << [d[idx],d[idx+1]];idx+=2};out << max_env(e).to_s
 end
 print out.join("\n\n")
end
