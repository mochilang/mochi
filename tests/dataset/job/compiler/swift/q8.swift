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

func test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing() {
  expect(result == [["actress_pseudonym": "Y. S.", "japanese_movie_dubbed": "Dubbed Film"]])
}

let aka_name = [["person_id": 1, "name": "Y. S."]]
let cast_info = [
  ["person_id": 1, "movie_id": 10, "note": "(voice: English version)", "role_id": 1000]
]
let company_name = [["id": 50, "country_code": "[jp]"]]
let movie_companies = [["movie_id": 10, "company_id": 50, "note": "Studio (Japan)"]]
let name = [["id": 1, "name": "Yoko Ono"], ["id": 2, "name": "Yuichi"]]
let role_type = [["id": 1000, "role": "actress"]]
let title = [["id": 10, "title": "Dubbed Film"]]
let eligible =
  ({
    var _res: [[String: Any]] = []
    for an1 in aka_name {
      for n1 in name {
        if !(n1["id"]! == an1["person_id"]!) { continue }
        for ci in cast_info {
          if !(ci["person_id"]! == an1["person_id"]!) { continue }
          for t in title {
            if !(t["id"]! == ci["movie_id"]!) { continue }
            for mc in movie_companies {
              if !(mc["movie_id"]! == ci["movie_id"]!) { continue }
              for cn in company_name {
                if !(cn["id"]! == mc["company_id"]!) { continue }
                for rt in role_type {
                  if !(rt["id"]! == ci["role_id"]!) { continue }
                  if !(ci["note"]! == "(voice: English version)" && cn["country_code"]! == "[jp]"
                    && mc["note"]!["contains"]!("(Japan)") && (!mc["note"]!["contains"]!("(USA)"))
                    && n1["name"]!["contains"]!("Yo") && (!n1["name"]!["contains"]!("Yu"))
                    && rt["role"]! == "actress")
                  {
                    continue
                  }
                  _res.append(["pseudonym": an1["name"]!, "movie_title": t["title"]!])
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
    "actress_pseudonym": _min(
      ({
        var _res: [Any] = []
        for x in eligible {
          _res.append(x["pseudonym"]!)
        }
        var _items = _res
        return _items
      }())),
    "japanese_movie_dubbed": _min(
      ({
        var _res: [Any] = []
        for x in eligible {
          _res.append(x["movie_title"]!)
        }
        var _items = _res
        return _items
      }())),
  ]
]
func main() {
  _json(result)
  test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing()
}
main()
