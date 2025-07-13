<?php
$keyword = [
    [
        "id" => 1,
        "keyword" => "10,000-mile-club"
    ],
    [
        "id" => 2,
        "keyword" => "character-name-in-title"
    ]
];
$link_type = [
    ["id" => 1, "link" => "sequel"],
    ["id" => 2, "link" => "remake"]
];
$movie_keyword = [
    ["movie_id" => 100, "keyword_id" => 1],
    ["movie_id" => 200, "keyword_id" => 2]
];
$movie_link = [
    [
        "movie_id" => 100,
        "linked_movie_id" => 300,
        "link_type_id" => 1
    ],
    [
        "movie_id" => 200,
        "linked_movie_id" => 400,
        "link_type_id" => 2
    ]
];
$title = [
    ["id" => 100, "title" => "Movie A"],
    ["id" => 200, "title" => "Movie B"],
    ["id" => 300, "title" => "Movie C"],
    ["id" => 400, "title" => "Movie D"]
];
$joined = (function() use ($keyword, $link_type, $movie_keyword, $movie_link, $title) {
    $result = [];
    foreach ($keyword as $k) {
        foreach ($movie_keyword as $mk) {
            if ($mk['keyword_id'] == $k['id']) {
                foreach ($title as $t1) {
                    if ($t1['id'] == $mk['movie_id']) {
                        foreach ($movie_link as $ml) {
                            if ($ml['movie_id'] == $t1['id']) {
                                foreach ($title as $t2) {
                                    if ($t2['id'] == $ml['linked_movie_id']) {
                                        foreach ($link_type as $lt) {
                                            if ($lt['id'] == $ml['link_type_id']) {
                                                if ($k['keyword'] == "10,000-mile-club") {
                                                    $result[] = [
    "link_type" => $lt['link'],
    "first_movie" => $t1['title'],
    "second_movie" => $t2['title']
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
    return $result;
})();
$result = [
    "link_type" => min((function() use ($joined) {
        $result = [];
        foreach ($joined as $r) {
            $result[] = $r['link_type'];
        }
        return $result;
    })()),
    "first_movie" => min((function() use ($joined) {
        $result = [];
        foreach ($joined as $r) {
            $result[] = $r['first_movie'];
        }
        return $result;
    })()),
    "second_movie" => min((function() use ($joined) {
        $result = [];
        foreach ($joined as $r) {
            $result[] = $r['second_movie'];
        }
        return $result;
    })())
];
echo json_encode([$result]), PHP_EOL;
?>
