def solve(m,k)
 r=m.length;c=m[0].length;best=-10**18
 (0...r).each do |top|
  sums=Array.new(c,0)
  (top...r).each do |bot|
   c.times{|j|sums[j]+=m[bot][j]}
   pre=[0];cur=0
   sums.each do |x|
    cur+=x;i=pre.bsearch_index{|v|v>=cur-k};best=[best,cur-pre[i]].max if i
    j=pre.bsearch_index{|v|v>=cur}||pre.length;pre.insert(j,cur)
   end
  end
 end
 best
end
d=STDIN.read.split.map(&:to_i);if d.length>0;idx=0;t=d[idx];idx+=1;out=[];t.times{r=d[idx];c=d[idx+1];idx+=2;m=[];r.times{m<<d[idx,c];idx+=c};k=d[idx];idx+=1;out<<solve(m,k).to_s};print out.join("\n\n");end
