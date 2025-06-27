import Foundation

func expect(_ cond: Bool) {
  if !cond { fatalError("expect failed") }
}

class _Group {
  var key: Any
  var Items: [Any] = []
  init(_ k: Any) { self.key = k }
}

func _json(_ v: Any) {
  if let d = try? JSONSerialization.data(withJSONObject: v, options: []),
    let s = String(data: d, encoding: .utf8)
  {
    print(s)
  }
}

func _min(_ v: Any) -> Any {
  var list: [Any]? = nil
  if let g = v as? _Group {
    list = g.Items
  } else if let arr = v as? [Any] {
    list = arr
  } else if let arr = v as? [Int] {
    return arr.min() ?? 0
  } else if let arr = v as? [Double] {
    return arr.min() ?? 0.0
  } else if let arr = v as? [String] {
    return arr.min() ?? ""
  }
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

func test_Q5_finds_the_lexicographically_first_qualifying_title() {
  expect(result == [["typical_european_movie": "A Film"]])
}

let company_type = [["ct_id": 1, "kind": "production companies"], ["ct_id": 2, "kind": "other"]]
let info_type = [["it_id": 10, "info": "languages"]]
let title = [
  ["t_id": 100, "title": "B Movie", "production_year": 2010],
  ["t_id": 200, "title": "A Film", "production_year": 2012],
  ["t_id": 300, "title": "Old Movie", "production_year": 2000],
]
let movie_companies = [
  ["movie_id": 100, "company_type_id": 1, "note": "ACME (France) (theatrical)"],
  ["movie_id": 200, "company_type_id": 1, "note": "ACME (France) (theatrical)"],
  ["movie_id": 300, "company_type_id": 1, "note": "ACME (France) (theatrical)"],
]
let movie_info = [
  ["movie_id": 100, "info": "German", "info_type_id": 10],
  ["movie_id": 200, "info": "Swedish", "info_type_id": 10],
  ["movie_id": 300, "info": "German", "info_type_id": 10],
]
let candidate_titles =
  ({
    var _res: [Any] = []
    for ct in company_type {
      for mc in movie_companies {
        if !(mc["company_type_id"]! == ct["ct_id"]!) { continue }
        for mi in movie_info {
          if !(mi["movie_id"]! == mc["movie_id"]!) { continue }
          for it in info_type {
            if !(it["it_id"]! == mi["info_type_id"]!) { continue }
            for t in title {
              if !(t["t_id"]! == mc["movie_id"]!) { continue }
              if !(mc["note"]!.contains(
                mc["note"]!.contains(ct["kind"]! == "production companies" && "(theatrical)")
                  && "(France)") && t["production_year"]! > 2005
                && ([
                  "Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian",
                  "German",
                ].contains(mi["info"]!)))
              {
                continue
              }
              _res.append(t["title"]!)
            }
          }
        }
      }
    }
    var _items = _res
    return _items
  }())
let result = [["typical_european_movie": _min(candidate_titles)]]
func main() {
  _json(result)
  test_Q5_finds_the_lexicographically_first_qualifying_title()
}
main()
