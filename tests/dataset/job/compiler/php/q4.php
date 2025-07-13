<?php
$info_type = [
    ["id" => 1, "info" => "rating"],
    ["id" => 2, "info" => "other"]
];
$keyword = [
    ["id" => 1, "keyword" => "great sequel"],
    ["id" => 2, "keyword" => "prequel"]
];
$title = [
    [
        "id" => 10,
        "title" => "Alpha Movie",
        "production_year" => 2006
    ],
    [
        "id" => 20,
        "title" => "Beta Film",
        "production_year" => 2007
    ],
    [
        "id" => 30,
        "title" => "Old Film",
        "production_year" => 2004
    ]
];
$movie_keyword = [
    ["movie_id" => 10, "keyword_id" => 1],
    ["movie_id" => 20, "keyword_id" => 1],
    ["movie_id" => 30, "keyword_id" => 1]
];
$movie_info_idx = [
    [
        "movie_id" => 10,
        "info_type_id" => 1,
        "info" => "6.2"
    ],
    [
        "movie_id" => 20,
        "info_type_id" => 1,
        "info" => "7.8"
    ],
    [
        "movie_id" => 30,
        "info_type_id" => 1,
        "info" => "4.5"
    ]
];
$rows = (function() use ($info_type, $keyword, $movie_info_idx, $movie_keyword, $title) {
    $result = [];
    foreach ($info_type as $it) {
        foreach ($movie_info_idx as $mi) {
            if ($it['id'] == $mi['info_type_id']) {
                foreach ($title as $t) {
                    if ($t['id'] == $mi['movie_id']) {
                        foreach ($movie_keyword as $mk) {
                            if ($mk['movie_id'] == $t['id']) {
                                foreach ($keyword as $k) {
                                    if ($k['id'] == $mk['keyword_id']) {
                                        if ($it['info'] == "rating" && strpos($k['keyword'], "sequel") !== false && $mi['info'] > "5.0" && $t['production_year'] > 2005 && $mk['movie_id'] == $mi['movie_id']) {
                                            $result[] = [
    "rating" => $mi['info'],
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
        "rating" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['rating'];
            }
            return $result;
        })()),
        "movie_title" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['title'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
