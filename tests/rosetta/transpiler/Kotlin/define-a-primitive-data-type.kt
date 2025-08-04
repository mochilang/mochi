data class TinyInt(var value: Int = 0) {
    fun Add(t2: TinyInt): TinyInt {
        return ((NewTinyInt((value as Int) + t2.value)) as TinyInt)
    }
    fun Sub(t2: TinyInt): TinyInt {
        return ((NewTinyInt((value as Int) - t2.value)) as TinyInt)
    }
    fun Mul(t2: TinyInt): TinyInt {
        return ((NewTinyInt((value as Int) * t2.value)) as TinyInt)
    }
    fun Div(t2: TinyInt): TinyInt {
        return ((NewTinyInt((value as Int) / t2.value)) as TinyInt)
    }
    fun Rem(t2: TinyInt): TinyInt {
        return ((NewTinyInt(Math.floorMod((value as Int), t2.value))) as TinyInt)
    }
    fun Inc(): TinyInt {
        return ((Add(NewTinyInt(1))) as TinyInt)
    }
    fun Dec(): TinyInt {
        return ((Sub(NewTinyInt(1))) as TinyInt)
    }
}
fun NewTinyInt(i: Int): TinyInt {
    var i: Int = i
    if (i < 1) {
        i = 1
    } else {
        if (i > 10) {
            i = 10
        }
    }
    return TinyInt(value = i)
}

fun user_main(): Unit {
    var t1: TinyInt = NewTinyInt(6)
    var t2: TinyInt = NewTinyInt(3)
    println("t1      = " + t1.value.toString())
    println("t2      = " + t2.value.toString())
    println("t1 + t2 = " + t1.Add(t2).value.toString())
    println("t1 - t2 = " + t1.Sub(t2).value.toString())
    println("t1 * t2 = " + t1.Mul(t2).value.toString())
    println("t1 / t2 = " + t1.Div(t2).value.toString())
    println("t1 % t2 = " + t1.Rem(t2).value.toString())
    println("t1 + 1  = " + t1.Inc().value.toString())
    println("t1 - 1  = " + t1.Dec().value.toString())
}

fun main() {
    user_main()
}
