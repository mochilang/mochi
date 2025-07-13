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

func test_Q3_returns_lexicographically_smallest_sequel_title() {
  expect(result == [["movie_title": "Alpha"]])
}

let keyword = [["id": 1, "keyword": "amazing sequel"], ["id": 2, "keyword": "prequel"]]
let movie_info = [
  ["movie_id": 10, "info": "Germany"], ["movie_id": 30, "info": "Sweden"],
  ["movie_id": 20, "info": "France"],
]
let movie_keyword: [[String: Int]] = [
  ["movie_id": 10, "keyword_id": 1], ["movie_id": 30, "keyword_id": 1],
  ["movie_id": 20, "keyword_id": 1], ["movie_id": 10, "keyword_id": 2],
]
let title = [
  ["id": 10, "title": "Alpha", "production_year": 2006],
  ["id": 30, "title": "Beta", "production_year": 2008],
  ["id": 20, "title": "Gamma", "production_year": 2009],
]
let allowed_infos: [String] = [
  "Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German",
]
let candidate_titles =
  ({
    var _res: [Any] = []
    for k in keyword {
      for mk in movie_keyword {
        if !(mk["keyword_id"]! == k["id"]!) { continue }
        for mi in movie_info {
          if !(mi["movie_id"]! == mk["movie_id"]!) { continue }
          for t in title {
            if !(t["id"]! == mi["movie_id"]!) { continue }
            if allowed_infos.contains(k["keyword"]!["contains"]!("sequel") && mi["info"]!)
              && t["production_year"]! > 2005 && mk["movie_id"]! == mi["movie_id"]!
            {
              _res.append(t["title"]!)
            }
          }
        }
      }
    }
    var _items = _res
    return _items
  }())
let result = [["movie_title": _min(candidate_titles)]]
func main() {
  _json(result)
  test_Q3_returns_lexicographically_smallest_sequel_title()
}
main()
