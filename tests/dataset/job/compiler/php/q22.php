<?php
$company_name = [
    [
        "id" => 1,
        "name" => "Euro Films",
        "country_code" => "[de]"
    ],
    [
        "id" => 2,
        "name" => "US Films",
        "country_code" => "[us]"
    ]
];
$company_type = [["id" => 1, "kind" => "production"]];
$info_type = [
    ["id" => 10, "info" => "countries"],
    ["id" => 20, "info" => "rating"]
];
$keyword = [
    ["id" => 1, "keyword" => "murder"],
    ["id" => 2, "keyword" => "comedy"]
];
$kind_type = [
    ["id" => 100, "kind" => "movie"],
    ["id" => 200, "kind" => "episode"]
];
$movie_companies = [
    [
        "movie_id" => 10,
        "company_id" => 1,
        "company_type_id" => 1,
        "note" => "release (2009) (worldwide)"
    ],
    [
        "movie_id" => 20,
        "company_id" => 2,
        "company_type_id" => 1,
        "note" => "release (2007) (USA)"
    ]
];
$movie_info = [
    [
        "movie_id" => 10,
        "info_type_id" => 10,
        "info" => "Germany"
    ],
    [
        "movie_id" => 20,
        "info_type_id" => 10,
        "info" => "USA"
    ]
];
$movie_info_idx = [
    [
        "movie_id" => 10,
        "info_type_id" => 20,
        "info" => 6.5
    ],
    [
        "movie_id" => 20,
        "info_type_id" => 20,
        "info" => 7.8
    ]
];
$movie_keyword = [
    ["movie_id" => 10, "keyword_id" => 1],
    ["movie_id" => 20, "keyword_id" => 2]
];
$title = [
    [
        "id" => 10,
        "kind_id" => 100,
        "production_year" => 2009,
        "title" => "Violent Western"
    ],
    [
        "id" => 20,
        "kind_id" => 100,
        "production_year" => 2007,
        "title" => "Old Western"
    ]
];
$rows = (function() use ($company_name, $company_type, $info_type, $keyword, $kind_type, $movie_companies, $movie_info, $movie_info_idx, $movie_keyword, $title) {
    $result = [];
    foreach ($company_name as $cn) {
        foreach ($movie_companies as $mc) {
            if ($cn['id'] == $mc['company_id']) {
                foreach ($company_type as $ct) {
                    if ($ct['id'] == $mc['company_type_id']) {
                        foreach ($title as $t) {
                            if ($t['id'] == $mc['movie_id']) {
                                foreach ($movie_keyword as $mk) {
                                    if ($mk['movie_id'] == $t['id']) {
                                        foreach ($keyword as $k) {
                                            if ($k['id'] == $mk['keyword_id']) {
                                                foreach ($movie_info as $mi) {
                                                    if ($mi['movie_id'] == $t['id']) {
                                                        foreach ($info_type as $it1) {
                                                            if ($it1['id'] == $mi['info_type_id']) {
                                                                foreach ($movie_info_idx as $mi_idx) {
                                                                    if ($mi_idx['movie_id'] == $t['id']) {
                                                                        foreach ($info_type as $it2) {
                                                                            if ($it2['id'] == $mi_idx['info_type_id']) {
                                                                                foreach ($kind_type as $kt) {
                                                                                    if ($kt['id'] == $t['kind_id']) {
                                                                                        if ((!($cn['country_code'] != "[us]" && $it1['info'] == "countries" && $it2['info'] == "rating" && ($k['keyword'] == "murder" || $k['keyword'] == "murder-in-title" || $k['keyword'] == "blood" || $k['keyword'] == "violence") && ($kt['kind'] == "movie" || $kt['kind'] == "episode") && strpos($mc['note'], "(USA)") !== false) && strpos($mc['note'], "(200") !== false && ($mi['info'] == "Germany" || $mi['info'] == "German" || $mi['info'] == "USA" || $mi['info'] == "American") && $mi_idx['info'] < 7 && $t['production_year'] > 2008 && $kt['id'] == $t['kind_id'] && $t['id'] == $mi['movie_id'] && $t['id'] == $mk['movie_id'] && $t['id'] == $mi_idx['movie_id'] && $t['id'] == $mc['movie_id'] && $mk['movie_id'] == $mi['movie_id'] && $mk['movie_id'] == $mi_idx['movie_id'] && $mk['movie_id'] == $mc['movie_id'] && $mi['movie_id'] == $mi_idx['movie_id'] && $mi['movie_id'] == $mc['movie_id'] && $mc['movie_id'] == $mi_idx['movie_id'] && $k['id'] == $mk['keyword_id'] && $it1['id'] == $mi['info_type_id'] && $it2['id'] == $mi_idx['info_type_id'] && $ct['id'] == $mc['company_type_id'] && $cn['id'] == $mc['company_id'])) {
                                                                                            $result[] = [
    "company" => $cn['name'],
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
        "movie_company" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['company'];
            }
            return $result;
        })()),
        "rating" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['rating'];
            }
            return $result;
        })()),
        "western_violent_movie" => min((function() use ($rows) {
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
