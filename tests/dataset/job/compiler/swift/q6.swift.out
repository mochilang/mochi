import Foundation

func expect(_ cond: Bool) {
  if !cond { fatalError("expect failed") }
}

func _json(_ v: Any) {
  if let d = try? JSONSerialization.data(withJSONObject: v, options: []),
    let s = String(data: d, encoding: .utf8)
  {
    print(s)
  }
}

func test_Q6_finds_marvel_movie_with_Robert_Downey() {
  expect(
    result == [
      [
        "movie_keyword": "marvel-cinematic-universe", "actor_name": "Downey Robert Jr.",
        "marvel_movie": "Iron Man 3",
      ]
    ])
}

let cast_info: [[String: Int]] = [
  ["movie_id": 1, "person_id": 101], ["movie_id": 2, "person_id": 102],
]
let keyword = [
  ["id": 100, "keyword": "marvel-cinematic-universe"], ["id": 200, "keyword": "other"],
]
let movie_keyword: [[String: Int]] = [
  ["movie_id": 1, "keyword_id": 100], ["movie_id": 2, "keyword_id": 200],
]
let name = [["id": 101, "name": "Downey Robert Jr."], ["id": 102, "name": "Chris Evans"]]
let title = [
  ["id": 1, "title": "Iron Man 3", "production_year": 2013],
  ["id": 2, "title": "Old Movie", "production_year": 2000],
]
let result =
  ({
    var _res: [[String: Any]] = []
    for ci in cast_info {
      for mk in movie_keyword {
        if !(ci["movie_id"]! == mk["movie_id"]!) { continue }
        for k in keyword {
          if !(mk["keyword_id"]! == k["id"]!) { continue }
          for n in name {
            if !(ci["person_id"]! == n["id"]!) { continue }
            for t in title {
              if !(ci["movie_id"]! == t["id"]!) { continue }
              if !(k["keyword"]! == "marvel-cinematic-universe" && n["name"]!["contains"]!("Downey")
                && n["name"]!["contains"]!("Robert") && t["production_year"]! > 2010)
              {
                continue
              }
              _res.append([
                "movie_keyword": k["keyword"]!, "actor_name": n["name"]!,
                "marvel_movie": t["title"]!,
              ])
            }
          }
        }
      }
    }
    var _items = _res
    return _items
  }())
func main() {
  _json(result)
  test_Q6_finds_marvel_movie_with_Robert_Downey()
}
main()
