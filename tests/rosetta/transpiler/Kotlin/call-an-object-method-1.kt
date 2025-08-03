class Foo {
    fun ValueMethod(x: Int): Unit {
    }
    fun PointerMethod(x: Int): Unit {
    }
}
var myValue: Foo = Foo()
var myPointer: Foo = Foo()
fun main() {
    myValue.ValueMethod(0)
    myPointer.PointerMethod(0)
    myPointer.ValueMethod(0)
    myValue.PointerMethod(0)
    myValue.ValueMethod(0)
    myPointer.PointerMethod(0)
}
