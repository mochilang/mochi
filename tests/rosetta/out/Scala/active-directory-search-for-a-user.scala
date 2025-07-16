object active_directory_search_for_a_user {
  def search_user(directory: Map[String, List[String]], username: String): List[String] = (directory).apply(username)
  
  def main() = {
    val client = Map("Base" -> "dc=example,dc=com", "Host" -> "ldap.example.com", "Port" -> 389, "GroupFilter" -> "(memberUid=%s)")
    val directory = Map("username" -> List("admins", "users"), "john" -> List("users"))
    val groups = search_user(directory, "username")
    if (groups.length > 0) {
      var out = "Groups: ["
      var i = 0
      while (i < groups.length) {
        out = out + "\"" + (groups).apply(i) + "\""
        if (i < groups.length - 1) {
          out += ", "
        }
        i += 1
      }
      out += "]"
      println(out)
    } else {
      println("User not found")
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
