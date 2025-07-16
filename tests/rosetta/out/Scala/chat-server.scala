object chat_server {
  def removeName(names: List[String], name: String): List[String] = {
    var out: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    for(n <- names) {
      if (n != name) {
        out = out :+ n
      }
    }
    return out
  }
  
  def main() = {
    var clients: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    def broadcast(msg: String) = {
      println(msg)
    }
    def add(name: String) = {
      clients = clients :+ name
      broadcast(("+++ \"" + name).asInstanceOf[Int] + "\" connected +++\n")
    }
    def send(name: String, msg: String) = {
      broadcast(((name + "> ").asInstanceOf[Int] + msg).asInstanceOf[Int] + "\n")
    }
    def remove(name: String) = {
      clients = removeName(clients, name)
      broadcast(("--- \"" + name).asInstanceOf[Int] + "\" disconnected ---\n")
    }
    add("Alice")
    add("Bob")
    send("Alice", "Hello Bob!")
    send("Bob", "Hi Alice!")
    remove("Bob")
    remove("Alice")
    broadcast("Server stopping!\n")
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
