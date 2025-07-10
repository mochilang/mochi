import org.yaml.snakeyaml.Yaml
import java.io.File

data class Person(val name: String, val age: Int, val email: String)

fun main() {
    val yaml = Yaml()
    val text = File("../interpreter/valid/people.yaml").readText()
    val raw: List<Map<String, Any>> = yaml.load(text)
    val people = raw.map { Person(it["name"] as String, (it["age"] as Number).toInt(), it["email"] as String) }
    val adults = people.filter { it.age >= 18 }
    for (a in adults) {
        println("${a.name} ${a.email}")
    }
}
