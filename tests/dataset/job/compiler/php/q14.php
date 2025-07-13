<?php
$info_type = [
    ["id" => 1, "info" => "countries"],
    ["id" => 2, "info" => "rating"]
];
$keyword = [
    ["id" => 1, "keyword" => "murder"],
    ["id" => 2, "keyword" => "blood"],
    ["id" => 3, "keyword" => "romance"]
];
$kind_type = [["id" => 1, "kind" => "movie"]];
$title = [
    [
        "id" => 1,
        "kind_id" => 1,
        "production_year" => 2012,
        "title" => "A Dark Movie"
    ],
    [
        "id" => 2,
        "kind_id" => 1,
        "production_year" => 2013,
        "title" => "Brutal Blood"
    ],
    [
        "id" => 3,
        "kind_id" => 1,
        "production_year" => 2008,
        "title" => "Old Film"
    ]
];
$movie_info = [
    [
        "movie_id" => 1,
        "info_type_id" => 1,
        "info" => "Sweden"
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 1,
        "info" => "USA"
    ],
    [
        "movie_id" => 3,
        "info_type_id" => 1,
        "info" => "USA"
    ]
];
$movie_info_idx = [
    [
        "movie_id" => 1,
        "info_type_id" => 2,
        "info" => 7
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 2,
        "info" => 7.5
    ],
    [
        "movie_id" => 3,
        "info_type_id" => 2,
        "info" => 9.1
    ]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 1],
    ["movie_id" => 2, "keyword_id" => 2],
    ["movie_id" => 3, "keyword_id" => 3]
];
$allowed_keywords = [
    "murder",
    "murder-in-title",
    "blood",
    "violence"
];
$allowed_countries = [
    "Sweden",
    "Norway",
    "Germany",
    "Denmark",
    "Swedish",
    "Denish",
    "Norwegian",
    "German",
    "USA",
    "American"
];
$matches = (function() use ($allowed_countries, $allowed_keywords, $info_type, $keyword, $kind_type, $movie_info, $movie_info_idx, $movie_keyword, $title) {
    $result = [];
    foreach ($info_type as $it1) {
        foreach ($info_type as $it2) {
            foreach ($keyword as $k) {
                foreach ($kind_type as $kt) {
                    foreach ($movie_info as $mi) {
                        foreach ($movie_info_idx as $mi_idx) {
                            foreach ($movie_keyword as $mk) {
                                foreach ($title as $t) {
                                    if (($it1['info'] == "countries" && $it2['info'] == "rating" && (in_array($k['keyword'], $allowed_keywords)) && $kt['kind'] == "movie" && (in_array($mi['info'], $allowed_countries)) && $mi_idx['info'] < 8.5 && $t['production_year'] > 2010 && $kt['id'] == $t['kind_id'] && $t['id'] == $mi['movie_id'] && $t['id'] == $mk['movie_id'] && $t['id'] == $mi_idx['movie_id'] && $mk['movie_id'] == $mi['movie_id'] && $mk['movie_id'] == $mi_idx['movie_id'] && $mi['movie_id'] == $mi_idx['movie_id'] && $k['id'] == $mk['keyword_id'] && $it1['id'] == $mi['info_type_id'] && $it2['id'] == $mi_idx['info_type_id'])) {
                                        $result[] = [
    "rating" => $mi_idx['info'],
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
    return $result;
})();
$result = [
    "rating" => min((function() use ($matches) {
        $result = [];
        foreach ($matches as $x) {
            $result[] = $x['rating'];
        }
        return $result;
    })()),
    "northern_dark_movie" => min((function() use ($matches) {
        $result = [];
        foreach ($matches as $x) {
            $result[] = $x['title'];
        }
        return $result;
    })())
];
echo json_encode($result), PHP_EOL;
?>
