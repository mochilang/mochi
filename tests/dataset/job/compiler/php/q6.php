<?php
$cast_info = [
    ["movie_id" => 1, "person_id" => 101],
    ["movie_id" => 2, "person_id" => 102]
];
$keyword = [
    [
        "id" => 100,
        "keyword" => "marvel-cinematic-universe"
    ],
    ["id" => 200, "keyword" => "other"]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 100],
    ["movie_id" => 2, "keyword_id" => 200]
];
$name = [
    [
        "id" => 101,
        "name" => "Downey Robert Jr."
    ],
    ["id" => 102, "name" => "Chris Evans"]
];
$title = [
    [
        "id" => 1,
        "title" => "Iron Man 3",
        "production_year" => 2013
    ],
    [
        "id" => 2,
        "title" => "Old Movie",
        "production_year" => 2000
    ]
];
$result = (function() use ($cast_info, $keyword, $movie_keyword, $name, $title) {
    $result = [];
    foreach ($cast_info as $ci) {
        foreach ($movie_keyword as $mk) {
            if ($ci['movie_id'] == $mk['movie_id']) {
                foreach ($keyword as $k) {
                    if ($mk['keyword_id'] == $k['id']) {
                        foreach ($name as $n) {
                            if ($ci['person_id'] == $n['id']) {
                                foreach ($title as $t) {
                                    if ($ci['movie_id'] == $t['id']) {
                                        if ($k['keyword'] == "marvel-cinematic-universe" && strpos($n['name'], "Downey") !== false && strpos($n['name'], "Robert") !== false && $t['production_year'] > 2010) {
                                            $result[] = [
    "movie_keyword" => $k['keyword'],
    "actor_name" => $n['name'],
    "marvel_movie" => $t['title']
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
echo json_encode($result), PHP_EOL;
?>
