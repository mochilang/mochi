// Generated by Mochi compiler v0.10.26 on 2006-01-02T15:04:05Z
import Foundation

func _json(_ v: Any) {
    func _sort(_ x: Any) -> Any {
        if let a = x as? [Any] { return a.map { _sort($0) } }
        if let m = x as? [String:Any] {
            var out: [String:Any] = [:]
            for k in m.keys.sorted() { out[k] = _sort(m[k]!) }
            return out
        }
        return x
    }
    let obj = _sort(v)
    if JSONSerialization.isValidJSONObject(obj),
       let data = try? JSONSerialization.data(withJSONObject: obj, options: [.sortedKeys]),
       let s = String(data: data, encoding: .utf8) {
        print(s)
    } else {
        print(obj)
    }
}
func expect(_ cond: Bool) {
    if !cond { fatalError("expect failed") }
}
func _structMap(_ v: Any) -> [String:Any]? {
    let mirror = Mirror(reflecting: v)
    if mirror.displayStyle == .struct || mirror.displayStyle == .class {
        var m: [String:Any] = [:]
        for child in mirror.children {
            if let k = child.label { m[k] = child.value }
        }
        return m
    }
    return nil
}
func _equal(_ a: Any, _ b: Any) -> Bool {
    if let am = _structMap(a), let bm = _structMap(b) {
        return _equal(am, bm)
    }
    if let am = _structMap(a), let bd = b as? [String: Any] {
        return _equal(am, bd)
    }
    if let ad = a as? [String: Any], let bm = _structMap(b) {
        return _equal(ad, bm)
    }
    switch (a, b) {
    case let (x as [Any], y as [Any]):
        if x.count != y.count { return false }
        for i in 0..<x.count {
            if !_equal(x[i], y[i]) { return false }
        }
        return true
    case let (x as [String: Any], y as [String: Any]):
        if x.count != y.count { return false }
        for (k, av) in x {
            guard let bv = y[k] else { return false }
            if !_equal(av, bv) { return false }
        }
        return true
    case let (ai as Double, bi as Int):
        return ai == Double(bi)
    case let (ai as Int, bi as Double):
        return Double(ai) == bi
    case let (ai as Double, bi as Double):
        return ai == bi
    case let (ai as Int, bi as Int):
        return ai == bi
    case let (sa as String, sb as String):
        return sa == sb
    case let (ab as Bool, bb as Bool):
        return ab == bb
    default:
        return false
    }
}
class _Group {
    var key: Any
    var Items: [Any] = []
    init(_ k: Any) { self.key = k }
}


func _keyStr(_ v: Any) -> String {
    if let data = try? JSONSerialization.data(withJSONObject: v, options: [.sortedKeys]),
       let s = String(data: data, encoding: .utf8) {
        return s
    }
    return String(describing: v)
}
func _min(_ v: Any) -> Any {
    var list: [Any]? = nil
    if let g = v as? _Group { list = g.Items }
    else if let arr = v as? [Any] { list = arr }
    else if let arr = v as? [Int] { return arr.min() ?? 0 }
    else if let arr = v as? [Double] { return arr.min() ?? 0.0 }
    else if let arr = v as? [String] { return arr.min() ?? "" }
    guard let items = list else { fatalError("min() expects list or group") }
    if items.isEmpty { return 0 }
    if let s = items[0] as? String {
        var m = s
        for it in items.dropFirst() {
            if let v = it as? String, v < m { m = v }
        }
        return m
    }
    func toDouble(_ v: Any) -> Double {
        if let i = v as? Int { return Double(i) }
        if let d = v as? Double { return d }
        if let f = v as? Float { return Double(f) }
        if let i = v as? Int64 { return Double(i) }
        return 0
    }
    var m = toDouble(items[0])
    var isFloat = items[0] is Double || items[0] is Float
    for it in items.dropFirst() {
        if it is Double || it is Float { isFloat = true }
        let d = toDouble(it)
        if d < m { m = d }
    }
    return isFloat ? m : Int(m)
}
struct Auto9: Equatable {
    var from_company: String
    var movie_link_type: String
    var non_polish_sequel_movie: String
}

struct CompanyName: Equatable {
    var country_code: String
    var id: Int
    var name: String
}

struct CompanyType: Equatable {
    var id: Int
    var kind: String
}

struct Keyword: Equatable {
    var id: Int
    var keyword: String
}

struct LinkType: Equatable {
    var id: Int
    var link: String
}

struct Matche: Equatable {
    var company: String
    var link: String
    var title: String
}

struct MovieKeyword: Equatable {
    var keyword_id: Int
    var movie_id: Int
}

struct MovieLink: Equatable {
    var link_type_id: Int
    var movie_id: Int
}

struct Title: Equatable {
    var id: Int
    var production_year: Int
    var title: String
}

var company_name = [CompanyName(country_code: "[us]", id: 1, name: "Best Film Co"), CompanyName(country_code: "[de]", id: 2, name: "Warner Studios"), CompanyName(country_code: "[pl]", id: 3, name: "Polish Films")]
var company_type = [CompanyType(id: 1, kind: "production companies"), CompanyType(id: 2, kind: "distributors")]
var keyword = [Keyword(id: 1, keyword: "sequel"), Keyword(id: 2, keyword: "thriller")]
var link_type = [LinkType(id: 1, link: "follow-up"), LinkType(id: 2, link: "follows from"), LinkType(id: 3, link: "remake")]
var movie_companies = [["movie_id": 10, "company_id": 1, "company_type_id": 1, "note": nil], ["movie_id": 20, "company_id": 2, "company_type_id": 1, "note": nil], ["movie_id": 30, "company_id": 3, "company_type_id": 1, "note": nil]]
var movie_keyword = [MovieKeyword(keyword_id: 1, movie_id: 10), MovieKeyword(keyword_id: 1, movie_id: 20), MovieKeyword(keyword_id: 2, movie_id: 20), MovieKeyword(keyword_id: 1, movie_id: 30)]
var movie_link = [MovieLink(link_type_id: 1, movie_id: 10), MovieLink(link_type_id: 2, movie_id: 20), MovieLink(link_type_id: 3, movie_id: 30)]
var title = [Title(id: 10, production_year: 1960, title: "Alpha"), Title(id: 20, production_year: 1970, title: "Beta"), Title(id: 30, production_year: 1985, title: "Polish Movie")]
var matches = ({
	var _res: [(company: String, link: String, title: String)] = []
	for cn in company_name {
		for mc in movie_companies {
			if !((mc["company_id"] as! Int) == cn.id) { continue }
			for ct in company_type {
				if !(ct.id == (mc["company_type_id"] as! Int)) { continue }
				for t in title {
					if !(t.id == (mc["movie_id"] as! Int)) { continue }
					for mk in movie_keyword {
						if !(mk.movie_id == t.id) { continue }
						for k in keyword {
							if !(k.id == mk.keyword_id) { continue }
							for ml in movie_link {
								if !(ml.movie_id == t.id) { continue }
								for lt in link_type {
									if !(lt.id == ml.link_type_id) { continue }
									if !(cn.country_code != "[pl]" && (cn.name.contains("Film") || cn.name.contains("Warner")) && ct.kind == "production companies" && k.keyword == "sequel" && lt.link.contains("follow") && mc["note"]! == nil && t.production_year >= 1950 && t.production_year <= 2000 && ml.movie_id == mk.movie_id && ml.movie_id == (mc["movie_id"] as! Int) && mk.movie_id == (mc["movie_id"] as! Int)) { continue }
									_res.append(["company": cn.name, "link": lt.link, "title": t.title])
								}
							}
						}
					}
				}
			}
		}
	}
	return _res
}())
var result = [["from_company": _min(matches.map { x in x.company }), "movie_link_type": _min(matches.map { x in x.link }), "non_polish_sequel_movie": _min(matches.map { x in x.title })]]
_json(result)
expect(_equal(result, [Auto9(from_company: "Best Film Co", movie_link_type: "follow-up", non_polish_sequel_movie: "Alpha")]))
