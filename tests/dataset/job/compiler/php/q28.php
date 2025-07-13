<?php
$comp_cast_type = [
    ["id" => 1, "kind" => "crew"],
    [
        "id" => 2,
        "kind" => "complete+verified"
    ],
    ["id" => 3, "kind" => "partial"]
];
$complete_cast = [
    [
        "movie_id" => 1,
        "subject_id" => 1,
        "status_id" => 3
    ],
    [
        "movie_id" => 2,
        "subject_id" => 1,
        "status_id" => 2
    ]
];
$company_name = [
    [
        "id" => 1,
        "name" => "Euro Films Ltd.",
        "country_code" => "[gb]"
    ],
    [
        "id" => 2,
        "name" => "US Studios",
        "country_code" => "[us]"
    ]
];
$company_type = [["id" => 1], ["id" => 2]];
$movie_companies = [
    [
        "movie_id" => 1,
        "company_id" => 1,
        "company_type_id" => 1,
        "note" => "production (2005) (UK)"
    ],
    [
        "movie_id" => 2,
        "company_id" => 2,
        "company_type_id" => 1,
        "note" => "production (USA)"
    ]
];
$info_type = [
    ["id" => 1, "info" => "countries"],
    ["id" => 2, "info" => "rating"]
];
$keyword = [
    ["id" => 1, "keyword" => "blood"],
    ["id" => 2, "keyword" => "romance"]
];
$kind_type = [
    ["id" => 1, "kind" => "movie"],
    ["id" => 2, "kind" => "episode"]
];
$movie_info = [
    [
        "movie_id" => 1,
        "info_type_id" => 1,
        "info" => "Germany"
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 1,
        "info" => "USA"
    ]
];
$movie_info_idx = [
    [
        "movie_id" => 1,
        "info_type_id" => 2,
        "info" => 7.2
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 2,
        "info" => 9
    ]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 1],
    ["movie_id" => 2, "keyword_id" => 2]
];
$title = [
    [
        "id" => 1,
        "kind_id" => 1,
        "production_year" => 2005,
        "title" => "Dark Euro Film"
    ],
    [
        "id" => 2,
        "kind_id" => 1,
        "production_year" => 2005,
        "title" => "US Film"
    ]
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
    "Danish",
    "Norwegian",
    "German",
    "USA",
    "American"
];
$matches = (function() use ($allowed_countries, $allowed_keywords, $comp_cast_type, $company_name, $company_type, $complete_cast, $info_type, $keyword, $kind_type, $movie_companies, $movie_info, $movie_info_idx, $movie_keyword, $title) {
    $result = [];
    foreach ($complete_cast as $cc) {
        foreach ($comp_cast_type as $cct1) {
            if ($cct1['id'] == $cc['subject_id']) {
                foreach ($comp_cast_type as $cct2) {
                    if ($cct2['id'] == $cc['status_id']) {
                        foreach ($movie_companies as $mc) {
                            if ($mc['movie_id'] == $cc['movie_id']) {
                                foreach ($company_name as $cn) {
                                    if ($cn['id'] == $mc['company_id']) {
                                        foreach ($company_type as $ct) {
                                            if ($ct['id'] == $mc['company_type_id']) {
                                                foreach ($movie_keyword as $mk) {
                                                    if ($mk['movie_id'] == $cc['movie_id']) {
                                                        foreach ($keyword as $k) {
                                                            if ($k['id'] == $mk['keyword_id']) {
                                                                foreach ($movie_info as $mi) {
                                                                    if ($mi['movie_id'] == $cc['movie_id']) {
                                                                        foreach ($info_type as $it1) {
                                                                            if ($it1['id'] == $mi['info_type_id']) {
                                                                                foreach ($movie_info_idx as $mi_idx) {
                                                                                    if ($mi_idx['movie_id'] == $cc['movie_id']) {
                                                                                        foreach ($info_type as $it2) {
                                                                                            if ($it2['id'] == $mi_idx['info_type_id']) {
                                                                                                foreach ($title as $t) {
                                                                                                    if ($t['id'] == $cc['movie_id']) {
                                                                                                        foreach ($kind_type as $kt) {
                                                                                                            if ($kt['id'] == $t['kind_id']) {
                                                                                                                if ((!($cct1['kind'] == "crew" && $cct2['kind'] != "complete+verified" && $cn['country_code'] != "[us]" && $it1['info'] == "countries" && $it2['info'] == "rating" && (in_array($k['keyword'], $allowed_keywords)) && (in_array($kt['kind'], ["movie", "episode"])) && strpos($mc['note'], "(USA)") !== false) && strpos($mc['note'], "(200") !== false && (in_array($mi['info'], $allowed_countries)) && $mi_idx['info'] < 8.5 && $t['production_year'] > 2000)) {
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
                        }
                    }
                }
            }
        }
    }
    return $result;
})();
$result = [
    "movie_company" => min((function() use ($matches) {
        $result = [];
        foreach ($matches as $x) {
            $result[] = $x['company'];
        }
        return $result;
    })()),
    "rating" => min((function() use ($matches) {
        $result = [];
        foreach ($matches as $x) {
            $result[] = $x['rating'];
        }
        return $result;
    })()),
    "complete_euro_dark_movie" => min((function() use ($matches) {
        $result = [];
        foreach ($matches as $x) {
            $result[] = $x['title'];
        }
        return $result;
    })())
];
echo json_encode($result), PHP_EOL;
?>
