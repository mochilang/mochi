<?php
$keyword = [
    [
        "id" => 1,
        "keyword" => "amazing sequel"
    ],
    ["id" => 2, "keyword" => "prequel"]
];
$movie_info = [
    ["movie_id" => 10, "info" => "Germany"],
    ["movie_id" => 30, "info" => "Sweden"],
    ["movie_id" => 20, "info" => "France"]
];
$movie_keyword = [
    ["movie_id" => 10, "keyword_id" => 1],
    ["movie_id" => 30, "keyword_id" => 1],
    ["movie_id" => 20, "keyword_id" => 1],
    ["movie_id" => 10, "keyword_id" => 2]
];
$title = [
    [
        "id" => 10,
        "title" => "Alpha",
        "production_year" => 2006
    ],
    [
        "id" => 30,
        "title" => "Beta",
        "production_year" => 2008
    ],
    [
        "id" => 20,
        "title" => "Gamma",
        "production_year" => 2009
    ]
];
$allowed_infos = [
    "Sweden",
    "Norway",
    "Germany",
    "Denmark",
    "Swedish",
    "Denish",
    "Norwegian",
    "German"
];
$candidate_titles = (function() use ($allowed_infos, $keyword, $movie_info, $movie_keyword, $title) {
    $result = [];
    foreach ($keyword as $k) {
        foreach ($movie_keyword as $mk) {
            if ($mk['keyword_id'] == $k['id']) {
                foreach ($movie_info as $mi) {
                    if ($mi['movie_id'] == $mk['movie_id']) {
                        foreach ($title as $t) {
                            if ($t['id'] == $mi['movie_id']) {
                                if (in_array(strpos($k['keyword'], "sequel") !== false && $mi['info'], $allowed_infos) && $t['production_year'] > 2005 && $mk['movie_id'] == $mi['movie_id']) {
                                    $result[] = $t['title'];
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
        "movie_title" => min($candidate_titles)
    ]
];
echo json_encode($result), PHP_EOL;
?>
