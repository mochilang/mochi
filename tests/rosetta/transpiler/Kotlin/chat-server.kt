fun removeName(names: MutableList<String>, name: String): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    for (n in names) {
        if (n != name) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(n); _tmp }
        }
    }
    return out
}

fun user_main(): Unit {
    var clients: MutableList<String> = mutableListOf<String>()
    fun broadcast(msg: String): Unit {
        println(msg)
    }

    fun add(name: String): Unit {
        clients = run { val _tmp = clients.toMutableList(); _tmp.add(name); _tmp }
        broadcast(("+++ \"" + name) + "\" connected +++\n")
    }

    fun send(name: String, msg: String): Unit {
        broadcast(((name + "> ") + msg) + "\n")
    }

    fun remove(name: String): Unit {
        clients = removeName(clients, name)
        broadcast(("--- \"" + name) + "\" disconnected ---\n")
    }

    add("Alice")
    add("Bob")
    send("Alice", "Hello Bob!")
    send("Bob", "Hi Alice!")
    remove("Bob")
    remove("Alice")
    broadcast("Server stopping!\n")
}

fun main() {
    user_main()
}
