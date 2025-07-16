Foobar = Struct.new(:Exported, :unexported, keyword_init: true)

def examineAndModify(f)
	puts(((((((((" v: {" + (f.Exported).to_s) + " ") + (f.unexported).to_s) + "} = {") + (f.Exported).to_s) + " ") + (f.unexported).to_s) + "}"))
	puts("    Idx Name       Type CanSet")
	puts("     0: Exported   int  true")
	puts("     1: unexported int  false")
	f.Exported = 16
	f.unexported = 44
	puts("  modified unexported field via unsafe")
	return f
end

def anotherExample()
	puts("bufio.ReadByte returned error: unsafely injected error value into bufio inner workings")
end

$obj = Foobar.new(Exported: 12, unexported: 42)
puts((((("obj: {" + ($obj.Exported).to_s) + " ") + ($obj.unexported).to_s) + "}"))
$obj = examineAndModify($obj)
puts((((("obj: {" + ($obj.Exported).to_s) + " ") + ($obj.unexported).to_s) + "}"))
anotherExample()
