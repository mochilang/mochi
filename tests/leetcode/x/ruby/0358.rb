def rearrange(s,k)
 return s if k <= 1
 c=Array.new(26,0); last=Array.new(26,-1_000_000_000); s.each_byte{|b| c[b-97]+=1}; out=[]
 s.length.times do |p|
  best=-1
  26.times{|i| best=i if c[i]>0 && p-last[i]>=k && (best<0 || c[i]>c[best])}
  return '' if best<0
  out << (97+best).chr; c[best]-=1; last[best]=p
 end
 out.join
end
d=STDIN.read.split
if d.length>0
 idx=0;t=d[idx].to_i;idx+=1;out=[]
 t.times{s=d[idx];k=d[idx+1].to_i;idx+=2;out << '"'+rearrange(s,k)+'"'}
 print out.join("\n\n")
end
