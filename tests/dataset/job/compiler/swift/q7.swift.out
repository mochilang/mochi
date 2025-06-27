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

func test_Q7_finds_movie_features_biography_for_person() {
  expect(result == [["of_person": "Alan Brown", "biography_movie": "Feature Film"]])
}

let aka_name = [["person_id": 1, "name": "Anna Mae"], ["person_id": 2, "name": "Chris"]]
let cast_info: [[String: Int]] = [
  ["person_id": 1, "movie_id": 10], ["person_id": 2, "movie_id": 20],
]
let info_type = [["id": 1, "info": "mini biography"], ["id": 2, "info": "trivia"]]
let link_type = [["id": 1, "link": "features"], ["id": 2, "link": "references"]]
let movie_link: [[String: Int]] = [
  ["linked_movie_id": 10, "link_type_id": 1], ["linked_movie_id": 20, "link_type_id": 2],
]
let name = [
  ["id": 1, "name": "Alan Brown", "name_pcode_cf": "B", "gender": "m"],
  ["id": 2, "name": "Zoe", "name_pcode_cf": "Z", "gender": "f"],
]
let person_info = [
  ["person_id": 1, "info_type_id": 1, "note": "Volker Boehm"],
  ["person_id": 2, "info_type_id": 1, "note": "Other"],
]
let title = [
  ["id": 10, "title": "Feature Film", "production_year": 1990],
  ["id": 20, "title": "Late Film", "production_year": 2000],
]
let rows =
  ({
    var _res: [[String: Any]] = []
    for an in aka_name {
      for n in name {
        if !(n["id"]! == an["person_id"]!) { continue }
        for pi in person_info {
          if !(pi["person_id"]! == an["person_id"]!) { continue }
          for it in info_type {
            if !(it["id"]! == pi["info_type_id"]!) { continue }
            for ci in cast_info {
              if !(ci["person_id"]! == n["id"]!) { continue }
              for t in title {
                if !(t["id"]! == ci["movie_id"]!) { continue }
                for ml in movie_link {
                  if !(ml["linked_movie_id"]! == t["id"]!) { continue }
                  for lt in link_type {
                    if !(lt["id"]! == ml["link_type_id"]!) { continue }
                    if !((an["name"]!["contains"]!("a") && it["info"]! == "mini biography"
                      && lt["link"]! == "features" && n["name_pcode_cf"]! >= "A"
                      && n["name_pcode_cf"]! <= "F"
                      && (n["gender"]! == "m"
                        || (n["gender"]! == "f" && n["name"]!["starts_with"]!("B")))
                      && pi["note"]! == "Volker Boehm" && t["production_year"]! >= 1980
                      && t["production_year"]! <= 1995 && pi["person_id"]! == an["person_id"]!
                      && pi["person_id"]! == ci["person_id"]!
                      && an["person_id"]! == ci["person_id"]!
                      && ci["movie_id"]! == ml["linked_movie_id"]!))
                    {
                      continue
                    }
                    _res.append(["person_name": n["name"]!, "movie_title": t["title"]!])
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
    "of_person": _min(
      ({
        var _res: [Any] = []
        for r in rows {
          _res.append(r["person_name"]!)
        }
        var _items = _res
        return _items
      }())),
    "biography_movie": _min(
      ({
        var _res: [Any] = []
        for r in rows {
          _res.append(r["movie_title"]!)
        }
        var _items = _res
        return _items
      }())),
  ]
]
func main() {
  _json(result)
  test_Q7_finds_movie_features_biography_for_person()
}
main()
