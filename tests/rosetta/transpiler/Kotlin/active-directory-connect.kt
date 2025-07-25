var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

data class LDAPClient(var Base: String, var Host: String, var Port: Int, var UseSSL: Boolean, var BindDN: String, var BindPassword: String, var UserFilter: String, var GroupFilter: String, var Attributes: MutableList<String>)
fun connect(client: LDAPClient): Boolean {
    return ((client.Host != "") && (client.Port > 0)) as Boolean
}

fun user_main(): Unit {
    val client: LDAPClient = LDAPClient(Base = "dc=example,dc=com", Host = "ldap.example.com", Port = 389, UseSSL = false, BindDN = "uid=readonlyuser,ou=People,dc=example,dc=com", BindPassword = "readonlypassword", UserFilter = "(uid=%s)", GroupFilter = "(memberUid=%s)", Attributes = mutableListOf("givenName", "sn", "mail", "uid"))
    if ((connect(client)) as Boolean) {
        println("Connected to " + client.Host)
    } else {
        println("Failed to connect")
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
