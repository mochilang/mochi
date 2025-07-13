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
        "note" => "(writer)"
    ]
];
$info_type = [
    ["id" => 1, "info" => "genres"],
    ["id" => 2, "info" => "votes"]
];
$keyword = [
    ["id" => 1, "keyword" => "murder"],
    ["id" => 2, "keyword" => "romance"]
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
        "info" => 100
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 2,
        "info" => 50
    ]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 1],
    ["movie_id" => 2, "keyword_id" => 2]
];
$name = [
    [
        "id" => 1,
        "name" => "Mike",
        "gender" => "m"
    ],
    [
        "id" => 2,
        "name" => "Sue",
        "gender" => "f"
    ]
];
$title = [
    ["id" => 1, "title" => "Scary Movie"],
    ["id" => 2, "title" => "Funny Movie"]
];
$allowed_notes = [
    "(writer)",
    "(head writer)",
    "(written by)",
    "(story)",
    "(story editor)"
];
$allowed_keywords = [
    "murder",
    "blood",
    "gore",
    "death",
    "female-nudity"
];
$matches = (function() use ($allowed_keywords, $allowed_notes, $cast_info, $info_type, $keyword, $movie_info, $movie_info_idx, $movie_keyword, $name, $title) {
    $result = [];
    foreach ($cast_info as $ci) {
        foreach ($info_type as $it1) {
            foreach ($info_type as $it2) {
                foreach ($keyword as $k) {
                    foreach ($movie_info as $mi) {
                        foreach ($movie_info_idx as $mi_idx) {
                            foreach ($movie_keyword as $mk) {
                                foreach ($name as $n) {
                                    foreach ($title as $t) {
                                        if (((in_array($ci['note'], $allowed_notes)) && $it1['info'] == "genres" && $it2['info'] == "votes" && (in_array($k['keyword'], $allowed_keywords)) && $mi['info'] == "Horror" && $n['gender'] == "m" && $t['id'] == $mi['movie_id'] && $t['id'] == $mi_idx['movie_id'] && $t['id'] == $ci['movie_id'] && $t['id'] == $mk['movie_id'] && $ci['movie_id'] == $mi['movie_id'] && $ci['movie_id'] == $mi_idx['movie_id'] && $ci['movie_id'] == $mk['movie_id'] && $mi['movie_id'] == $mi_idx['movie_id'] && $mi['movie_id'] == $mk['movie_id'] && $mi_idx['movie_id'] == $mk['movie_id'] && $n['id'] == $ci['person_id'] && $it1['id'] == $mi['info_type_id'] && $it2['id'] == $mi_idx['info_type_id'] && $k['id'] == $mk['keyword_id'])) {
                                            $result[] = [
    "budget" => $mi['info'],
    "votes" => $mi_idx['info'],
    "writer" => $n['name'],
    "title" => $t['title']
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
        "male_writer" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['writer'];
            }
            return $result;
        })()),
        "violent_movie_title" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['title'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
