<?php
$comp_cast_type = [
    ["id" => 1, "kind" => "cast"],
    [
        "id" => 2,
        "kind" => "complete+verified"
    ],
    ["id" => 3, "kind" => "crew"]
];
$complete_cast = [
    [
        "movie_id" => 1,
        "subject_id" => 1,
        "status_id" => 2
    ],
    [
        "movie_id" => 2,
        "subject_id" => 3,
        "status_id" => 2
    ]
];
$cast_info = [
    [
        "movie_id" => 1,
        "person_id" => 10,
        "note" => "(writer)"
    ],
    [
        "movie_id" => 2,
        "person_id" => 11,
        "note" => "(actor)"
    ]
];
$info_type = [
    ["id" => 1, "info" => "genres"],
    ["id" => 2, "info" => "votes"]
];
$keyword = [
    ["id" => 1, "keyword" => "murder"],
    ["id" => 2, "keyword" => "comedy"]
];
$movie_info = [
    [
        "movie_id" => 1,
        "info_type_id" => 1,
        "info" => "Horror"
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 1,
        "info" => "Comedy"
    ]
];
$movie_info_idx = [
    [
        "movie_id" => 1,
        "info_type_id" => 2,
        "info" => 2000
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 2,
        "info" => 150
    ]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 1],
    ["movie_id" => 2, "keyword_id" => 2]
];
$name = [
    [
        "id" => 10,
        "name" => "John Writer",
        "gender" => "m"
    ],
    [
        "id" => 11,
        "name" => "Jane Actor",
        "gender" => "f"
    ]
];
$title = [
    [
        "id" => 1,
        "title" => "Violent Horror",
        "production_year" => 2005
    ],
    [
        "id" => 2,
        "title" => "Old Comedy",
        "production_year" => 1995
    ]
];
$violent_keywords = [
    "murder",
    "violence",
    "blood",
    "gore",
    "death",
    "female-nudity",
    "hospital"
];
$writer_notes = [
    "(writer)",
    "(head writer)",
    "(written by)",
    "(story)",
    "(story editor)"
];
$matches = (function() use ($cast_info, $comp_cast_type, $complete_cast, $info_type, $keyword, $movie_info, $movie_info_idx, $movie_keyword, $name, $title, $violent_keywords, $writer_notes) {
    $result = [];
    foreach ($complete_cast as $cc) {
        foreach ($comp_cast_type as $cct1) {
            if ($cct1['id'] == $cc['subject_id']) {
                foreach ($comp_cast_type as $cct2) {
                    if ($cct2['id'] == $cc['status_id']) {
                        foreach ($cast_info as $ci) {
                            if ($ci['movie_id'] == $cc['movie_id']) {
                                foreach ($movie_info as $mi) {
                                    if ($mi['movie_id'] == $cc['movie_id']) {
                                        foreach ($movie_info_idx as $mi_idx) {
                                            if ($mi_idx['movie_id'] == $cc['movie_id']) {
                                                foreach ($movie_keyword as $mk) {
                                                    if ($mk['movie_id'] == $cc['movie_id']) {
                                                        foreach ($info_type as $it1) {
                                                            if ($it1['id'] == $mi['info_type_id']) {
                                                                foreach ($info_type as $it2) {
                                                                    if ($it2['id'] == $mi_idx['info_type_id']) {
                                                                        foreach ($keyword as $k) {
                                                                            if ($k['id'] == $mk['keyword_id']) {
                                                                                foreach ($name as $n) {
                                                                                    if ($n['id'] == $ci['person_id']) {
                                                                                        foreach ($title as $t) {
                                                                                            if ($t['id'] == $cc['movie_id']) {
                                                                                                if ((in_array($cct1['kind'], ["cast", "crew"])) && $cct2['kind'] == "complete+verified" && (in_array($ci['note'], $writer_notes)) && $it1['info'] == "genres" && $it2['info'] == "votes" && (in_array($k['keyword'], $violent_keywords)) && (in_array($mi['info'], ["Horror", "Thriller"])) && $n['gender'] == "m" && $t['production_year'] > 2000) {
                                                                                                    $result[] = [
    "budget" => $mi['info'],
    "votes" => $mi_idx['info'],
    "writer" => $n['name'],
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
        "movie_budget" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['budget'];
            }
            return $result;
        })()),
        "movie_votes" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['votes'];
            }
            return $result;
        })()),
        "writer" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['writer'];
            }
            return $result;
        })()),
        "complete_violent_movie" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['movie'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
