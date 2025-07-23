fun search_user(directory: MutableMap<String, MutableList<String>>, username: String): MutableList<String> {
    return (directory)[username] as MutableList<String>
}

fun user_main(): Unit {
    val client: MutableMap<String, Any> = mutableMapOf<String, Any>("Base" to ("dc=example,dc=com"), "Host" to ("ldap.example.com"), "Port" to (389), "GroupFilter" to ("(memberUid=%s)"))
    val directory: MutableMap<String, MutableList<String>> = mutableMapOf<String, MutableList<String>>("username" to (mutableListOf("admins", "users")), "john" to (mutableListOf("users")))
    val groups: MutableList<String> = search_user(directory as MutableMap<String, MutableList<String>>, "username") as MutableList<String>
    if (groups.size > 0) {
        var out: String = "Groups: ["
        var i: Int = 0
        while (i < groups.size) {
            out = ((out + "\"") + groups[i]) + "\""
            if (i < (groups.size - 1)) {
                out = out + ", "
            }
            i = i + 1
        }
        out = out + "]"
        println(out)
    } else {
        println("User not found")
    }
}

fun main() {
    user_main()
}
