data class LDAPClient(val Base: String, val Host: String, val Port: Int, val UseSSL: Boolean, val BindDN: String, val BindPassword: String, val UserFilter: String, val GroupFilter: String, val Attributes: MutableList<String>)
fun connect(client: LDAPClient): Boolean {
    return (client.Host != "") && (client.Port > 0) as Boolean
}

fun user_main(): Unit {
    val client: LDAPClient = LDAPClient(Base = "dc=example,dc=com", Host = "ldap.example.com", Port = 389, UseSSL = false, BindDN = "uid=readonlyuser,ou=People,dc=example,dc=com", BindPassword = "readonlypassword", UserFilter = "(uid=%s)", GroupFilter = "(memberUid=%s)", Attributes = mutableListOf("givenName", "sn", "mail", "uid"))
    if (connect(client as LDAPClient) as Boolean as Boolean) {
        println("Connected to " + (client.Host).toString())
    } else {
        println("Failed to connect")
    }
}

fun main() {
    user_main()
}
