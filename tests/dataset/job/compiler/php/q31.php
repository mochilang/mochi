<?php
$cast_info = [
    [
        "movie_id" => 1,
        "person_id" => 1,
        "note" => "(writer)"
    ],
    [
        "movie_id" => 2,
        "person_id" => 2,
        "note" => "(story)"
    ],
    [
        "movie_id" => 3,
        "person_id" => 3,
        "note" => "(writer)"
    ]
];
$company_name = [
    [
        "id" => 1,
        "name" => "Lionsgate Pictures"
    ],
    ["id" => 2, "name" => "Other Studio"]
];
$info_type = [
    ["id" => 10, "info" => "genres"],
    ["id" => 20, "info" => "votes"]
];
$keyword = [
    ["id" => 100, "keyword" => "murder"],
    ["id" => 200, "keyword" => "comedy"]
];
$movie_companies = [
    ["movie_id" => 1, "company_id" => 1],
    ["movie_id" => 2, "company_id" => 1],
    ["movie_id" => 3, "company_id" => 2]
];
$movie_info = [
    [
        "movie_id" => 1,
        "info_type_id" => 10,
        "info" => "Horror"
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 10,
        "info" => "Thriller"
    ],
    [
        "movie_id" => 3,
        "info_type_id" => 10,
        "info" => "Comedy"
    ]
];
$movie_info_idx = [
    [
        "movie_id" => 1,
        "info_type_id" => 20,
        "info" => 1000
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 20,
        "info" => 800
    ],
    [
        "movie_id" => 3,
        "info_type_id" => 20,
        "info" => 500
    ]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 100],
    ["movie_id" => 2, "keyword_id" => 100],
    ["movie_id" => 3, "keyword_id" => 200]
];
$name = [
    [
        "id" => 1,
        "name" => "Arthur",
        "gender" => "m"
    ],
    [
        "id" => 2,
        "name" => "Bob",
        "gender" => "m"
    ],
    [
        "id" => 3,
        "name" => "Carla",
        "gender" => "f"
    ]
];
$title = [
    ["id" => 1, "title" => "Alpha Horror"],
    ["id" => 2, "title" => "Beta Blood"],
    ["id" => 3, "title" => "Gamma Comedy"]
];
$matches = (function() use ($cast_info, $company_name, $info_type, $keyword, $movie_companies, $movie_info, $movie_info_idx, $movie_keyword, $name, $title) {
    $result = [];
    foreach ($cast_info as $ci) {
        foreach ($name as $n) {
            if ($n['id'] == $ci['person_id']) {
                foreach ($title as $t) {
                    if ($t['id'] == $ci['movie_id']) {
                        foreach ($movie_info as $mi) {
                            if ($mi['movie_id'] == $t['id']) {
                                foreach ($movie_info_idx as $mi_idx) {
                                    if ($mi_idx['movie_id'] == $t['id']) {
                                        foreach ($movie_keyword as $mk) {
                                            if ($mk['movie_id'] == $t['id']) {
                                                foreach ($keyword as $k) {
                                                    if ($k['id'] == $mk['keyword_id']) {
                                                        foreach ($movie_companies as $mc) {
                                                            if ($mc['movie_id'] == $t['id']) {
                                                                foreach ($company_name as $cn) {
                                                                    if ($cn['id'] == $mc['company_id']) {
                                                                        foreach ($info_type as $it1) {
                                                                            if ($it1['id'] == $mi['info_type_id']) {
                                                                                foreach ($info_type as $it2) {
                                                                                    if ($it2['id'] == $mi_idx['info_type_id']) {
                                                                                        if (in_array(in_array(in_array($ci['note'], [
    "(writer)",
    "(head writer)",
    "(written by)",
    "(story)",
    "(story editor)"
]) && str_starts_with($cn['name'], "Lionsgate") && $it1['info'] == "genres" && $it2['info'] == "votes" && $k['keyword'], [
    "murder",
    "violence",
    "blood",
    "gore",
    "death",
    "female-nudity",
    "hospital"
]) && $mi['info'], ["Horror", "Thriller"]) && $n['gender'] == "m") {
                                                                                            $result[] = [
    "movie_budget" => $mi['info'],
    "movie_votes" => $mi_idx['info'],
    "writer" => $n['name'],
    "violent_liongate_movie" => $t['title']
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
    return $result;
})();
$result = [
    [
        "movie_budget" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $r) {
                $result[] = $r['movie_budget'];
            }
            return $result;
        })()),
        "movie_votes" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $r) {
                $result[] = $r['movie_votes'];
            }
            return $result;
        })()),
        "writer" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $r) {
                $result[] = $r['writer'];
            }
            return $result;
        })()),
        "violent_liongate_movie" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $r) {
                $result[] = $r['violent_liongate_movie'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
