<?php
$aka_name = [
    ["person_id" => 1, "name" => "Alpha"],
    ["person_id" => 2, "name" => "Beta"]
];
$cast_info = [
    ["person_id" => 1, "movie_id" => 101],
    ["person_id" => 2, "movie_id" => 102]
];
$company_name = [
    ["id" => 1, "country_code" => "[us]"],
    ["id" => 2, "country_code" => "[de]"]
];
$keyword = [
    [
        "id" => 1,
        "keyword" => "character-name-in-title"
    ],
    ["id" => 2, "keyword" => "other"]
];
$movie_companies = [
    ["movie_id" => 101, "company_id" => 1],
    ["movie_id" => 102, "company_id" => 2]
];
$movie_keyword = [
    ["movie_id" => 101, "keyword_id" => 1],
    ["movie_id" => 102, "keyword_id" => 2]
];
$name = [["id" => 1], ["id" => 2]];
$title = [
    [
        "id" => 101,
        "title" => "Hero Bob",
        "episode_nr" => 60
    ],
    [
        "id" => 102,
        "title" => "Other Show",
        "episode_nr" => 40
    ]
];
$rows = (function() use ($aka_name, $cast_info, $company_name, $keyword, $movie_companies, $movie_keyword, $name, $title) {
    $result = [];
    foreach ($aka_name as $an) {
        foreach ($name as $n) {
            if ($n['id'] == $an['person_id']) {
                foreach ($cast_info as $ci) {
                    if ($ci['person_id'] == $n['id']) {
                        foreach ($title as $t) {
                            if ($t['id'] == $ci['movie_id']) {
                                foreach ($movie_keyword as $mk) {
                                    if ($mk['movie_id'] == $t['id']) {
                                        foreach ($keyword as $k) {
                                            if ($k['id'] == $mk['keyword_id']) {
                                                foreach ($movie_companies as $mc) {
                                                    if ($mc['movie_id'] == $t['id']) {
                                                        foreach ($company_name as $cn) {
                                                            if ($cn['id'] == $mc['company_id']) {
                                                                if ($cn['country_code'] == "[us]" && $k['keyword'] == "character-name-in-title" && $t['episode_nr'] >= 50 && $t['episode_nr'] < 100) {
                                                                    $result[] = [
    "pseudonym" => $an['name'],
    "series" => $t['title']
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
    return $result;
})();
$result = [
    [
        "cool_actor_pseudonym" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['pseudonym'];
            }
            return $result;
        })()),
        "series_named_after_char" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['series'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
