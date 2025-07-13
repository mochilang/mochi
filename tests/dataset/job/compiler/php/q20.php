<?php
$comp_cast_type = [
    ["id" => 1, "kind" => "cast"],
    ["id" => 2, "kind" => "complete cast"]
];
$char_name = [
    ["id" => 1, "name" => "Tony Stark"],
    ["id" => 2, "name" => "Sherlock Holmes"]
];
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
$name = [
    [
        "id" => 1,
        "name" => "Robert Downey Jr."
    ],
    ["id" => 2, "name" => "Another Actor"]
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
$keyword = [
    ["id" => 10, "keyword" => "superhero"],
    ["id" => 20, "keyword" => "romance"]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 10],
    ["movie_id" => 2, "keyword_id" => 20]
];
$kind_type = [["id" => 1, "kind" => "movie"]];
$title = [
    [
        "id" => 1,
        "kind_id" => 1,
        "production_year" => 2008,
        "title" => "Iron Man"
    ],
    [
        "id" => 2,
        "kind_id" => 1,
        "production_year" => 1940,
        "title" => "Old Hero"
    ]
];
$matches = (function() use ($cast_info, $char_name, $comp_cast_type, $complete_cast, $keyword, $kind_type, $movie_keyword, $name, $title) {
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
                                                foreach ($movie_keyword as $mk) {
                                                    if ($mk['movie_id'] == $cc['movie_id']) {
                                                        foreach ($keyword as $k) {
                                                            if ($k['id'] == $mk['keyword_id']) {
                                                                foreach ($title as $t) {
                                                                    if ($t['id'] == $cc['movie_id']) {
                                                                        foreach ($kind_type as $kt) {
                                                                            if ($kt['id'] == $t['kind_id']) {
                                                                                if (in_array($cct1['kind'] == "cast" && strpos($cct2['kind'], "complete") !== false && (!strpos($chn['name'], "Sherlock") !== false) && (strpos($chn['name'], "Tony Stark") !== false || strpos($chn['name'], "Iron Man") !== false) && $k['keyword'], [
    "superhero",
    "sequel",
    "second-part",
    "marvel-comics",
    "based-on-comic",
    "tv-special",
    "fight",
    "violence"
]) && $kt['kind'] == "movie" && $t['production_year'] > 1950) {
                                                                                    $result[] = $t['title'];
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
        "complete_downey_ironman_movie" => min($matches)
    ]
];
echo json_encode($result), PHP_EOL;
?>
