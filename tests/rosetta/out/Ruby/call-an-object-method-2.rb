Box = Struct.new(:Contents, :secret, keyword_init: true) do
	def TellSecret()
		return secret
	end
	
end

def newFactory()
	sn = 0
	New = ->(){
		sn = (sn + 1)
		b = Box.new(secret: sn)
		if (sn == 1)
			b.Contents = "rabbit"
		elsif (sn == 2)
			b.Contents = "rock"
		end
		return b
	}
	Count = ->(){
		return sn
	}
	return [$New, $Count]
end

$funcs = newFactory()
$New = $funcs[0]
$Count = $funcs[1]
