<?php
$aka_title = [["movie_id" => 1], ["movie_id" => 2]];
$company_name = [
    ["id" => 1, "country_code" => "[us]"],
    ["id" => 2, "country_code" => "[gb]"]
];
$company_type = [["id" => 10], ["id" => 20]];
$info_type = [
    ["id" => 5, "info" => "release dates"],
    ["id" => 6, "info" => "other"]
];
$keyword = [["id" => 100], ["id" => 200]];
$movie_companies = [
    [
        "movie_id" => 1,
        "company_id" => 1,
        "company_type_id" => 10,
        "note" => "release (2005) (worldwide)"
    ],
    [
        "movie_id" => 2,
        "company_id" => 2,
        "company_type_id" => 20,
        "note" => "release (1999) (worldwide)"
    ]
];
$movie_info = [
    [
        "movie_id" => 1,
        "info_type_id" => 5,
        "note" => "internet",
        "info" => "USA: March 2005"
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 5,
        "note" => "theater",
        "info" => "USA: May 1999"
    ]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 100],
    ["movie_id" => 2, "keyword_id" => 200]
];
$title = [
    [
        "id" => 1,
        "title" => "Example Movie",
        "production_year" => 2005
    ],
    [
        "id" => 2,
        "title" => "Old Movie",
        "production_year" => 1999
    ]
];
$rows = (function() use ($aka_title, $company_name, $company_type, $info_type, $keyword, $movie_companies, $movie_info, $movie_keyword, $title) {
    $result = [];
    foreach ($title as $t) {
        foreach ($aka_title as $at) {
            if ($at['movie_id'] == $t['id']) {
                foreach ($movie_info as $mi) {
                    if ($mi['movie_id'] == $t['id']) {
                        foreach ($movie_keyword as $mk) {
                            if ($mk['movie_id'] == $t['id']) {
                                foreach ($movie_companies as $mc) {
                                    if ($mc['movie_id'] == $t['id']) {
                                        foreach ($keyword as $k) {
                                            if ($k['id'] == $mk['keyword_id']) {
                                                foreach ($info_type as $it1) {
                                                    if ($it1['id'] == $mi['info_type_id']) {
                                                        foreach ($company_name as $cn) {
                                                            if ($cn['id'] == $mc['company_id']) {
                                                                foreach ($company_type as $ct) {
                                                                    if ($ct['id'] == $mc['company_type_id']) {
                                                                        if ($cn['country_code'] == "[us]" && $it1['info'] == "release dates" && strpos($mc['note'], "200") !== false && strpos($mc['note'], "worldwide") !== false && strpos($mi['note'], "internet") !== false && strpos($mi['info'], "USA:") !== false && strpos($mi['info'], "200") !== false && $t['production_year'] > 2000) {
                                                                            $result[] = [
    "release_date" => $mi['info'],
    "internet_movie" => $t['title']
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
    return $result;
})();
$result = [
    [
        "release_date" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['release_date'];
            }
            return $result;
        })()),
        "internet_movie" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['internet_movie'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
