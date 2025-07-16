Box = Struct.new(:Contents, :secret, keyword_init: true) do
	def TellSecret()
		return secret
	end
	
end

def New()
	b = Box.new(Contents: "rabbit", secret: 1)
	return b
end

$box = New()
$box.TellSecret.call()
