<?php
$complete_cast = [
    [
        "movie_id" => 1,
        "subject_id" => 1,
        "status_id" => 2
    ],
    [
        "movie_id" => 2,
        "subject_id" => 1,
        "status_id" => 2
    ]
];
$comp_cast_type = [
    ["id" => 1, "kind" => "cast"],
    ["id" => 2, "kind" => "complete"]
];
$char_name = [
    ["id" => 1, "name" => "Spider-Man"],
    ["id" => 2, "name" => "Villain"]
];
$cast_info = [
    [
        "movie_id" => 1,
        "person_role_id" => 1,
        "person_id" => 1
    ],
    [
        "movie_id" => 2,
        "person_role_id" => 2,
        "person_id" => 2
    ]
];
$info_type = [["id" => 1, "info" => "rating"]];
$keyword = [
    ["id" => 1, "keyword" => "superhero"],
    ["id" => 2, "keyword" => "comedy"]
];
$kind_type = [["id" => 1, "kind" => "movie"]];
$movie_info_idx = [
    [
        "movie_id" => 1,
        "info_type_id" => 1,
        "info" => 8.5
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 1,
        "info" => 6.5
    ]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 1],
    ["movie_id" => 2, "keyword_id" => 2]
];
$name = [
    ["id" => 1, "name" => "Actor One"],
    ["id" => 2, "name" => "Actor Two"]
];
$title = [
    [
        "id" => 1,
        "kind_id" => 1,
        "production_year" => 2005,
        "title" => "Hero Movie"
    ],
    [
        "id" => 2,
        "kind_id" => 1,
        "production_year" => 1999,
        "title" => "Old Film"
    ]
];
$allowed_keywords = [
    "superhero",
    "marvel-comics",
    "based-on-comic",
    "tv-special",
    "fight",
    "violence",
    "magnet",
    "web",
    "claw",
    "laser"
];
$rows = (function() use ($allowed_keywords, $cast_info, $char_name, $comp_cast_type, $complete_cast, $info_type, $keyword, $kind_type, $movie_info_idx, $movie_keyword, $name, $title) {
    $result = [];
    foreach ($complete_cast as $cc) {
        foreach ($comp_cast_type as $cct1) {
            if ($cct1['id'] == $cc['subject_id']) {
                foreach ($comp_cast_type as $cct2) {
                    if ($cct2['id'] == $cc['status_id']) {
                        foreach ($cast_info as $ci) {
                            if ($ci['movie_id'] == $cc['movie_id']) {
                                foreach ($char_name as $chn) {
                                    if ($chn['id'] == $ci['person_role_id']) {
                                        foreach ($name as $n) {
                                            if ($n['id'] == $ci['person_id']) {
                                                foreach ($title as $t) {
                                                    if ($t['id'] == $ci['movie_id']) {
                                                        foreach ($kind_type as $kt) {
                                                            if ($kt['id'] == $t['kind_id']) {
                                                                foreach ($movie_keyword as $mk) {
                                                                    if ($mk['movie_id'] == $t['id']) {
                                                                        foreach ($keyword as $k) {
                                                                            if ($k['id'] == $mk['keyword_id']) {
                                                                                foreach ($movie_info_idx as $mi_idx) {
                                                                                    if ($mi_idx['movie_id'] == $t['id']) {
                                                                                        foreach ($info_type as $it2) {
                                                                                            if ($it2['id'] == $mi_idx['info_type_id']) {
                                                                                                if ($cct1['kind'] == "cast" && strpos($cct2['kind'], "complete") !== false && $chn['name'] != null && (strpos($chn['name'], "man") !== false || strpos($chn['name'], "Man") !== false) && $it2['info'] == "rating" && (in_array($k['keyword'], $allowed_keywords)) && $kt['kind'] == "movie" && $mi_idx['info'] > 7 && $t['production_year'] > 2000) {
                                                                                                    $result[] = [
    "character" => $chn['name'],
    "rating" => $mi_idx['info'],
    "actor" => $n['name'],
    "movie" => $t['title']
];
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return $result;
})();
$result = [
    [
        "character_name" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['character'];
            }
            return $result;
        })()),
        "rating" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['rating'];
            }
            return $result;
        })()),
        "playing_actor" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['actor'];
            }
            return $result;
        })()),
        "complete_hero_movie" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['movie'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
