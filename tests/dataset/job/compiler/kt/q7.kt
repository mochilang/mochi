// Generated by Mochi compiler v0.10.25 on 2025-07-13T13:02:47Z
fun min(list: List<Any?>): Any? {
    if (list.isEmpty()) return 0
    var m = list[0]
    for (n in list) {
        if ((n as Comparable<Any?>) < (m as Comparable<Any?>)) m = n
    }
    return m
}

fun String.starts_with(prefix: String): Boolean = this.startsWith(prefix)

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}

fun json(v: Any?) {
    println(toJson(v))
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}
// Code generated from q7.mochi

data class Aka_name(var person_id: Int, var name: String)

data class Cast_info(var person_id: Int, var movie_id: Int)

data class Info_type(var id: Int, var info: String)

data class Link_type(var id: Int, var link: String)

data class Movie_link(var linked_movie_id: Int, var link_type_id: Int)

data class Name(var id: Int, var name: String, var name_pcode_cf: String, var gender: String)

data class Person_info(var person_id: Int, var info_type_id: Int, var note: String)

data class Result(var of_person: Any?, var biography_movie: Any?)

data class Title(var id: Int, var title: String, var production_year: Int)

val aka_name = mutableListOf(Aka_name(person_id = 1, name = "Anna Mae"), Aka_name(person_id = 2, name = "Chris"))

val cast_info = mutableListOf(Cast_info(person_id = 1, movie_id = 10), Cast_info(person_id = 2, movie_id = 20))

val info_type = mutableListOf(Info_type(id = 1, info = "mini biography"), Info_type(id = 2, info = "trivia"))

val link_type = mutableListOf(Link_type(id = 1, link = "features"), Link_type(id = 2, link = "references"))

val movie_link = mutableListOf(Movie_link(linked_movie_id = 10, link_type_id = 1), Movie_link(linked_movie_id = 20, link_type_id = 2))

val name = mutableListOf(Name(id = 1, name = "Alan Brown", name_pcode_cf = "B", gender = "m"), Name(id = 2, name = "Zoe", name_pcode_cf = "Z", gender = "f"))

val person_info = mutableListOf(Person_info(person_id = 1, info_type_id = 1, note = "Volker Boehm"), Person_info(person_id = 2, info_type_id = 1, note = "Other"))

val title = mutableListOf(Title(id = 10, title = "Feature Film", production_year = 1990), Title(id = 20, title = "Late Film", production_year = 2000))

val rows = run {
    val __res = mutableListOf<Any?>()
    for (an in aka_name) {
        for (n in name) {
            if (n.id == an.person_id) {
                for (pi in person_info) {
                    if (pi.person_id == an.person_id) {
                        for (it in info_type) {
                            if (it.id == pi.info_type_id) {
                                for (ci in cast_info) {
                                    if (ci.person_id == n.id) {
                                        for (t in title) {
                                            if (t.id == ci.movie_id) {
                                                for (ml in movie_link) {
                                                    if (ml.linked_movie_id == t.id) {
                                                        for (lt in link_type) {
                                                            if (lt.id == ml.link_type_id) {
                                                                if (toBool((an.name.contains("a") && it.info == "mini biography" && lt.link == "features" && n.name_pcode_cf >= "A" && n.name_pcode_cf <= "F" && (n.gender == "m" || (n.gender == "f" && n.name.starts_with("B"))) && pi.note == "Volker Boehm" && t.production_year >= 1980 && t.production_year <= 1995 && pi.person_id == an.person_id && pi.person_id == ci.person_id && an.person_id == ci.person_id && ci.movie_id == ml.linked_movie_id))) {
                                                                    __res.add(mutableMapOf("person_name" to n.name, "movie_title" to t.title))
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    __res
}

val result = mutableListOf(Result(of_person = min(run {
    val __res = mutableListOf<Any?>()
    for (r in rows) {
        __res.add((r as MutableMap<String, Any?>)["person_name"])
    }
    __res
}), biography_movie = min(run {
    val __res = mutableListOf<Any?>()
    for (r in rows) {
        __res.add((r as MutableMap<String, Any?>)["movie_title"])
    }
    __res
})))

fun main() {
    json(result)
    check(result == mutableListOf(Result(of_person = "Alan Brown", biography_movie = "Feature Film")))
}
