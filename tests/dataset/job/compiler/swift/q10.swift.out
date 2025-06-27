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

func test_Q10_finds_uncredited_voice_actor_in_Russian_movie() {
  expect(result == [["uncredited_voiced_character": "Ivan", "russian_movie": "Vodka Dreams"]])
}

let char_name = [["id": 1, "name": "Ivan"], ["id": 2, "name": "Alex"]]
let cast_info = [
  ["movie_id": 10, "person_role_id": 1, "role_id": 1, "note": "Soldier (voice) (uncredited)"],
  ["movie_id": 11, "person_role_id": 2, "role_id": 1, "note": "(voice)"],
]
let company_name = [["id": 1, "country_code": "[ru]"], ["id": 2, "country_code": "[us]"]]
let company_type: [[String: Int]] = [["id": 1], ["id": 2]]
let movie_companies: [[String: Int]] = [
  ["movie_id": 10, "company_id": 1, "company_type_id": 1],
  ["movie_id": 11, "company_id": 2, "company_type_id": 1],
]
let role_type = [["id": 1, "role": "actor"], ["id": 2, "role": "director"]]
let title = [
  ["id": 10, "title": "Vodka Dreams", "production_year": 2006],
  ["id": 11, "title": "Other Film", "production_year": 2004],
]
let matches =
  ({
    var _res: [[String: Any]] = []
    for chn in char_name {
      for ci in cast_info {
        if !(chn["id"]! == ci["person_role_id"]!) { continue }
        for rt in role_type {
          if !(rt["id"]! == ci["role_id"]!) { continue }
          for t in title {
            if !(t["id"]! == ci["movie_id"]!) { continue }
            for mc in movie_companies {
              if !(mc["movie_id"]! == t["id"]!) { continue }
              for cn in company_name {
                if !(cn["id"]! == mc["company_id"]!) { continue }
                if !(ci["note"]!["contains"]!("(voice)") && ci["note"]!["contains"]!("(uncredited)")
                  && cn["country_code"]! == "[ru]" && rt["role"]! == "actor"
                  && t["production_year"]! > 2005)
                {
                  continue
                }
                for ct in company_type {
                  if !(ct["id"]! == mc["company_type_id"]!) { continue }
                  _res.append(["character": chn["name"]!, "movie": t["title"]!])
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
    "uncredited_voiced_character": _min(
      ({
        var _res: [Any] = []
        for x in matches {
          _res.append(x["character"]!)
        }
        var _items = _res
        return _items
      }())),
    "russian_movie": _min(
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
  test_Q10_finds_uncredited_voice_actor_in_Russian_movie()
}
main()
