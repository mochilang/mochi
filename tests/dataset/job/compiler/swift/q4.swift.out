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

func test_Q4_returns_minimum_rating_and_title_for_sequels() {
  expect(result == [["rating": "6.2", "movie_title": "Alpha Movie"]])
}

let info_type = [["id": 1, "info": "rating"], ["id": 2, "info": "other"]]
let keyword = [["id": 1, "keyword": "great sequel"], ["id": 2, "keyword": "prequel"]]
let title = [
  ["id": 10, "title": "Alpha Movie", "production_year": 2006],
  ["id": 20, "title": "Beta Film", "production_year": 2007],
  ["id": 30, "title": "Old Film", "production_year": 2004],
]
let movie_keyword: [[String: Int]] = [
  ["movie_id": 10, "keyword_id": 1], ["movie_id": 20, "keyword_id": 1],
  ["movie_id": 30, "keyword_id": 1],
]
let movie_info_idx = [
  ["movie_id": 10, "info_type_id": 1, "info": "6.2"],
  ["movie_id": 20, "info_type_id": 1, "info": "7.8"],
  ["movie_id": 30, "info_type_id": 1, "info": "4.5"],
]
let rows =
  ({
    var _res: [[String: Any]] = []
    for it in info_type {
      for mi in movie_info_idx {
        if !(it["id"]! == mi["info_type_id"]!) { continue }
        for t in title {
          if !(t["id"]! == mi["movie_id"]!) { continue }
          for mk in movie_keyword {
            if !(mk["movie_id"]! == t["id"]!) { continue }
            for k in keyword {
              if !(k["id"]! == mk["keyword_id"]!) { continue }
              if !(it["info"]! == "rating" && k["keyword"]!["contains"]!("sequel")
                && mi["info"]! > "5.0" && t["production_year"]! > 2005
                && mk["movie_id"]! == mi["movie_id"]!)
              {
                continue
              }
              _res.append(["rating": mi["info"]!, "title": t["title"]!])
            }
          }
        }
      }
    }
    var _items = _res
    return _items
  }())
let result = [
  [
    "rating": _min(
      ({
        var _res: [Any] = []
        for r in rows {
          _res.append(r["rating"]!)
        }
        var _items = _res
        return _items
      }())),
    "movie_title": _min(
      ({
        var _res: [Any] = []
        for r in rows {
          _res.append(r["title"]!)
        }
        var _items = _res
        return _items
      }())),
  ]
]
func main() {
  _json(result)
  test_Q4_returns_minimum_rating_and_title_for_sequels()
}
main()
