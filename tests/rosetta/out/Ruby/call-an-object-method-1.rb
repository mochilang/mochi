Foo = Struct.new(keyword_init: true) do
	def ValueMethod(x)
	end
	
	def PointerMethod(x)
	end
	
end

$myValue = Foo.new()
$myPointer = Foo.new()
$myValue.ValueMethod.call(0)
$myPointer.PointerMethod.call(0)
$myPointer.ValueMethod.call(0)
$myValue.PointerMethod.call(0)
$myValue.ValueMethod.call(0)
$myPointer.PointerMethod.call(0)
