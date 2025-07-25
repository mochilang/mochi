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
struct CompanyName: Equatable {
    var country_code: String
    var id: Int
    var name: String
}

struct CompanyType: Equatable {
    var id: Int
    var kind: String
}

struct InfoType: Equatable {
    var id: Int
    var info: String
}

struct MovieCompany: Equatable {
    var company_id: Int
    var company_type_id: Int
    var movie_id: Int
}

struct MovieInfo: Equatable {
    var info: String
    var info_type_id: Int
    var movie_id: Int
}

struct MovieInfoIdx: Equatable {
    var info: Int
    var info_type_id: Int
    var movie_id: Int
}

struct Result: Equatable {
    var drama_horror_movie: String
    var movie_company: String
    var rating: Int
}

struct Title: Equatable {
    var id: Int
    var production_year: Int
    var title: String
}

var company_name = [CompanyName(country_code: "[us]", id: 1, name: "Best Pictures"), CompanyName(country_code: "[uk]", id: 2, name: "Foreign Films")]
var company_type = [CompanyType(id: 10, kind: "production companies"), CompanyType(id: 20, kind: "distributors")]
var info_type = [InfoType(id: 100, info: "genres"), InfoType(id: 200, info: "rating")]
var movie_companies = [MovieCompany(company_id: 1, company_type_id: 10, movie_id: 1000), MovieCompany(company_id: 2, company_type_id: 10, movie_id: 2000)]
var movie_info = [MovieInfo(info: "Drama", info_type_id: 100, movie_id: 1000), MovieInfo(info: "Horror", info_type_id: 100, movie_id: 2000)]
var movie_info_idx = [MovieInfoIdx(info: 8.3, info_type_id: 200, movie_id: 1000), MovieInfoIdx(info: 7.5, info_type_id: 200, movie_id: 2000)]
var title = [Title(id: 1000, production_year: 2006, title: "Great Drama"), Title(id: 2000, production_year: 2007, title: "Low Rated")]
var result = ({
	var _res: [(drama_horror_movie: String, movie_company: String, rating: Int)] = []
	for cn in company_name {
		for mc in movie_companies {
			if !(mc.company_id == cn.id) { continue }
			for ct in company_type {
				if !(ct.id == mc.company_type_id) { continue }
				for t in title {
					if !(t.id == mc.movie_id) { continue }
					for mi in movie_info {
						if !(mi.movie_id == t.id) { continue }
						for it1 in info_type {
							if !(it1.id == mi.info_type_id) { continue }
							for mi_idx in movie_info_idx {
								if !(mi_idx.movie_id == t.id) { continue }
								for it2 in info_type {
									if !(it2.id == mi_idx.info_type_id) { continue }
									if !(cn.country_code == "[us]" && ct.kind == "production companies" && it1.info == "genres" && it2.info == "rating" && (mi.info == "Drama" || mi.info == "Horror") && mi_idx.info > 8.0 && t.production_year >= 2005 && t.production_year <= 2008) { continue }
									_res.append(["movie_company": cn.name, "rating": mi_idx.info, "drama_horror_movie": t.title])
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
_json(result)
expect(_equal(result, [Result(drama_horror_movie: "Great Drama", movie_company: "Best Pictures", rating: 8.3)]))
