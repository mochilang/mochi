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

func test_Q9_selects_minimal_alternative_name__character_and_movie() {
  expect(
    result == [["alternative_name": "A. N. G.", "character_name": "Angel", "movie": "Famous Film"]])
}

let aka_name = [["person_id": 1, "name": "A. N. G."], ["person_id": 2, "name": "J. D."]]
let char_name = [["id": 10, "name": "Angel"], ["id": 20, "name": "Devil"]]
let cast_info = [
  ["person_id": 1, "person_role_id": 10, "movie_id": 100, "role_id": 1000, "note": "(voice)"],
  ["person_id": 2, "person_role_id": 20, "movie_id": 200, "role_id": 1000, "note": "(voice)"],
]
let company_name = [["id": 100, "country_code": "[us]"], ["id": 200, "country_code": "[gb]"]]
let movie_companies = [
  ["movie_id": 100, "company_id": 100, "note": "ACME Studios (USA)"],
  ["movie_id": 200, "company_id": 200, "note": "Maple Films"],
]
let name = [
  ["id": 1, "name": "Angela Smith", "gender": "f"], ["id": 2, "name": "John Doe", "gender": "m"],
]
let role_type = [["id": 1000, "role": "actress"], ["id": 2000, "role": "actor"]]
let title = [
  ["id": 100, "title": "Famous Film", "production_year": 2010],
  ["id": 200, "title": "Old Movie", "production_year": 1999],
]
let matches =
  ({
    var _res: [[String: Any]] = []
    for an in aka_name {
      for n in name {
        if !(an["person_id"]! == n["id"]!) { continue }
        for ci in cast_info {
          if !(ci["person_id"]! == n["id"]!) { continue }
          for chn in char_name {
            if !(chn["id"]! == ci["person_role_id"]!) { continue }
            for t in title {
              if !(t["id"]! == ci["movie_id"]!) { continue }
              for mc in movie_companies {
                if !(mc["movie_id"]! == t["id"]!) { continue }
                for cn in company_name {
                  if !(cn["id"]! == mc["company_id"]!) { continue }
                  for rt in role_type {
                    if !(rt["id"]! == ci["role_id"]!) { continue }
                    if !(([
                      "(voice)", "(voice: Japanese version)", "(voice) (uncredited)",
                      "(voice: English version)",
                    ].contains(ci["note"]!)) && cn["country_code"]! == "[us]"
                      && (mc["note"]!["contains"]!("(USA)")
                        || mc["note"]!["contains"]!("(worldwide)"))
                      && n["gender"]! == "f" && n["name"]!["contains"]!("Ang")
                      && rt["role"]! == "actress" && t["production_year"]! >= 2005
                      && t["production_year"]! <= 2015)
                    {
                      continue
                    }
                    _res.append([
                      "alt": an["name"]!, "character": chn["name"]!, "movie": t["title"]!,
                    ])
                  }
                }
              }
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
    "alternative_name": _min(
      ({
        var _res: [Any] = []
        for x in matches {
          _res.append(x["alt"]!)
        }
        var _items = _res
        return _items
      }())),
    "character_name": _min(
      ({
        var _res: [Any] = []
        for x in matches {
          _res.append(x["character"]!)
        }
        var _items = _res
        return _items
      }())),
    "movie": _min(
      ({
        var _res: [Any] = []
        for x in matches {
          _res.append(x["movie"]!)
        }
        var _items = _res
        return _items
      }())),
  ]
]
func main() {
  _json(result)
  test_Q9_selects_minimal_alternative_name__character_and_movie()
}
main()
