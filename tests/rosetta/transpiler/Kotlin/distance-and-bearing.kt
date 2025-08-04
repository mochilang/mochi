var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
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

data class Airport(var name: String = "", var country: String = "", var icao: String = "", var lat: Double = 0.0, var lon: Double = 0.0)
var PI: Double = 3.141592653589793
var airports: MutableList<Airport> = mutableListOf(Airport(name = "Koksijde Air Base", country = "Belgium", icao = "EBFN", lat = 51.090301513671875, lon = 2.652780055999756), Airport(name = "Ostend-Bruges International Airport", country = "Belgium", icao = "EBOS", lat = 51.198898315399994, lon = 2.8622200489), Airport(name = "Kent International Airport", country = "United Kingdom", icao = "EGMH", lat = 51.342201, lon = 1.34611), Airport(name = "Calais-Dunkerque Airport", country = "France", icao = "LFAC", lat = 50.962100982666016, lon = 1.954759955406189), Airport(name = "Westkapelle heliport", country = "Belgium", icao = "EBKW", lat = 51.32222366333, lon = 3.2930560112), Airport(name = "Lympne Airport", country = "United Kingdom", icao = "EGMK", lat = 51.08, lon = 1.013), Airport(name = "Ursel Air Base", country = "Belgium", icao = "EBUL", lat = 51.14419937133789, lon = 3.475559949874878), Airport(name = "Southend Airport", country = "United Kingdom", icao = "EGMC", lat = 51.5713996887207, lon = 0.6955559849739075), Airport(name = "Merville-Calonne Airport", country = "France", icao = "LFQT", lat = 50.61840057373047, lon = 2.642240047454834), Airport(name = "Wevelgem Airport", country = "Belgium", icao = "EBKT", lat = 50.817199707, lon = 3.20472002029), Airport(name = "Midden-Zeeland Airport", country = "Netherlands", icao = "EHMZ", lat = 51.5121994019, lon = 3.73111009598), Airport(name = "Lydd Airport", country = "United Kingdom", icao = "EGMD", lat = 50.95610046386719, lon = 0.9391670227050781), Airport(name = "RAF Wattisham", country = "United Kingdom", icao = "EGUW", lat = 52.1273002625, lon = 0.956264019012), Airport(name = "Beccles Airport", country = "United Kingdom", icao = "EGSM", lat = 52.435298919699996, lon = 1.6183300018300002), Airport(name = "Lille/Marcq-en-Baroeul Airport", country = "France", icao = "LFQO", lat = 50.687198638916016, lon = 3.0755600929260254), Airport(name = "Lashenden (Headcorn) Airfield", country = "United Kingdom", icao = "EGKH", lat = 51.156898, lon = 0.641667), Airport(name = "Le Touquet-Côte d'Opale Airport", country = "France", icao = "LFAT", lat = 50.517398834228516, lon = 1.6205899715423584), Airport(name = "Rochester Airport", country = "United Kingdom", icao = "EGTO", lat = 51.351898193359375, lon = 0.5033329725265503), Airport(name = "Lille-Lesquin Airport", country = "France", icao = "LFQQ", lat = 50.563332, lon = 3.086886), Airport(name = "Thurrock Airfield", country = "United Kingdom", icao = "EGMT", lat = 51.537505, lon = 0.367634))
fun sinApprox(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var n: Int = 1
    while (n <= 8) {
        var denom: Double = (((2 * n) * ((2 * n) + 1)).toDouble())
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun cosApprox(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = 1
    while (n <= 8) {
        var denom: Double = ((((2 * n) - 1) * (2 * n)).toDouble())
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun atanApprox(x: Double): Double {
    if (x > 1.0) {
        return (PI / 2.0) - (x / ((x * x) + 0.28))
    }
    if (x < (0.0 - 1.0)) {
        return ((0.0 - PI) / 2.0) - (x / ((x * x) + 0.28))
    }
    return x / (1.0 + ((0.28 * x) * x))
}

fun atan2Approx(y: Double, x: Double): Double {
    if (x > 0.0) {
        var r: Double = atanApprox(y / x)
        return r
    }
    if (x < 0.0) {
        if (y >= 0.0) {
            return atanApprox(y / x) + PI
        }
        return atanApprox(y / x) - PI
    }
    if (y > 0.0) {
        return PI / 2.0
    }
    if (y < 0.0) {
        return (0.0 - PI) / 2.0
    }
    return 0.0
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = 0
    while (i < 10) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun rad(x: Double): Double {
    return (x * PI) / 180.0
}

fun deg(x: Double): Double {
    return (x * 180.0) / PI
}

fun distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double {
    var phi1: Double = rad(lat1)
    var phi2: Double = rad(lat2)
    var dphi: Double = rad(lat2 - lat1)
    var dlambda: Double = rad(lon2 - lon1)
    var sdphi: Double = sinApprox(dphi / 2)
    var sdlambda: Double = sinApprox(dlambda / 2)
    var a: Double = (sdphi * sdphi) + (((cosApprox(phi1) * cosApprox(phi2)) * sdlambda) * sdlambda)
    var c: Double = 2 * atan2Approx(sqrtApprox(a), sqrtApprox(1 - a))
    return (6371.0 / 1.852) * c
}

fun bearing(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double {
    var phi1: Double = rad(lat1)
    var phi2: Double = rad(lat2)
    var dl: Double = rad(lon2 - lon1)
    var y: Double = sinApprox(dl) * cosApprox(phi2)
    var x: Double = (cosApprox(phi1) * sinApprox(phi2)) - ((sinApprox(phi1) * cosApprox(phi2)) * cosApprox(dl))
    var br: Double = deg(atan2Approx(y, x))
    if (br < 0) {
        br = br + 360
    }
    return br
}

fun floor(x: Double): Double {
    var i: Int = (x.toInt())
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
}

fun pow10(n: Int): Double {
    var p: Double = 1.0
    var i: Int = 0
    while (i < n) {
        p = p * 10.0
        i = i + 1
    }
    return p
}

fun round(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    return floor((x * m) + 0.5) / m
}

fun sortByDistance(xs: MutableList<MutableList<Any?>>): MutableList<MutableList<Any?>> {
    var arr: MutableList<MutableList<Any?>> = xs
    var i: Int = 1
    while (i < arr.size) {
        var j: Int = i
        while ((j > 0) && (((((arr[j - 1] as MutableList<Any?>) as MutableList<Any?>))[0] as Any? as Number).toDouble() > ((((arr[j] as MutableList<Any?>) as MutableList<Any?>))[0] as Any? as Number).toDouble())) {
            var tmp: MutableList<Any?> = arr[j - 1] as MutableList<Any?>
            arr[j - 1] = arr[j] as MutableList<Any?>
            arr[j] = tmp
            j = j - 1
        }
        i = i + 1
    }
    return arr
}

fun user_main(): Unit {
    var planeLat: Double = 51.514669
    var planeLon: Double = 2.198581
    var results: MutableList<MutableList<Any?>> = mutableListOf<MutableList<Any?>>()
    for (ap in airports) {
        var d: Double = distance(planeLat, planeLon, ap.lat, ap.lon)
        var b: Double = bearing(planeLat, planeLon, ap.lat, ap.lon)
        results = run { val _tmp = results.toMutableList(); _tmp.add(mutableListOf<Any?>((d as Any?), (b as Any?), (ap as Any?))); _tmp }
    }
    results = sortByDistance(results)
    println("Distance Bearing ICAO Country               Airport")
    println("--------------------------------------------------------------")
    var i: Int = 0
    while (i < results.size) {
        var r: MutableList<Any?> = results[i] as MutableList<Any?>
        var ap: Any? = r[2] as Any?
        var dist: Any? = r[0] as Any?
        var bear: Any? = r[1] as Any?
        var line: String = (((((((round((dist as Double), 1).toString() + "\t") + round((bear as Double), 0).toString()) + "\t") + ((ap as Airport)).icao) + "\t") + ((ap as Airport)).country) + " ") + ((ap as Airport)).name
        println(line)
        i = i + 1
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
