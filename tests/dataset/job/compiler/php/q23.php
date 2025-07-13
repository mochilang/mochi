<?php
$complete_cast = [
    ["movie_id" => 1, "status_id" => 1],
    ["movie_id" => 2, "status_id" => 2]
];
$comp_cast_type = [
    [
        "id" => 1,
        "kind" => "complete+verified"
    ],
    ["id" => 2, "kind" => "partial"]
];
$company_name = [
    ["id" => 1, "country_code" => "[us]"],
    ["id" => 2, "country_code" => "[gb]"]
];
$company_type = [["id" => 1], ["id" => 2]];
$info_type = [
    ["id" => 1, "info" => "release dates"],
    ["id" => 2, "info" => "other"]
];
$keyword = [
    ["id" => 1, "keyword" => "internet"],
    ["id" => 2, "keyword" => "other"]
];
$kind_type = [
    ["id" => 1, "kind" => "movie"],
    ["id" => 2, "kind" => "series"]
];
$movie_companies = [
    [
        "movie_id" => 1,
        "company_id" => 1,
        "company_type_id" => 1
    ],
    [
        "movie_id" => 2,
        "company_id" => 2,
        "company_type_id" => 2
    ]
];
$movie_info = [
    [
        "movie_id" => 1,
        "info_type_id" => 1,
        "note" => "internet release",
        "info" => "USA: May 2005"
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 1,
        "note" => "theater",
        "info" => "USA: April 1998"
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
        "title" => "Web Movie"
    ],
    [
        "id" => 2,
        "kind_id" => 1,
        "production_year" => 1998,
        "title" => "Old Movie"
    ]
];
$matches = (function() use ($comp_cast_type, $company_name, $company_type, $complete_cast, $info_type, $keyword, $kind_type, $movie_companies, $movie_info, $movie_keyword, $title) {
    $result = [];
    foreach ($complete_cast as $cc) {
        foreach ($comp_cast_type as $cct1) {
            if ($cct1['id'] == $cc['status_id']) {
                foreach ($title as $t) {
                    if ($t['id'] == $cc['movie_id']) {
                        foreach ($kind_type as $kt) {
                            if ($kt['id'] == $t['kind_id']) {
                                foreach ($movie_info as $mi) {
                                    if ($mi['movie_id'] == $t['id']) {
                                        foreach ($info_type as $it1) {
                                            if ($it1['id'] == $mi['info_type_id']) {
                                                foreach ($movie_keyword as $mk) {
                                                    if ($mk['movie_id'] == $t['id']) {
                                                        foreach ($keyword as $k) {
                                                            if ($k['id'] == $mk['keyword_id']) {
                                                                foreach ($movie_companies as $mc) {
                                                                    if ($mc['movie_id'] == $t['id']) {
                                                                        foreach ($company_name as $cn) {
                                                                            if ($cn['id'] == $mc['company_id']) {
                                                                                foreach ($company_type as $ct) {
                                                                                    if ($ct['id'] == $mc['company_type_id']) {
                                                                                        if ($cct1['kind'] == "complete+verified" && $cn['country_code'] == "[us]" && $it1['info'] == "release dates" && $kt['kind'] == "movie" && strpos($mi['note'], "internet") !== false && (strpos($mi['info'], "USA:") !== false && (strpos($mi['info'], "199") !== false || strpos($mi['info'], "200") !== false)) && $t['production_year'] > 2000) {
                                                                                            $result[] = [
    "movie_kind" => $kt['kind'],
    "complete_us_internet_movie" => $t['title']
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
        "movie_kind" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $r) {
                $result[] = $r['movie_kind'];
            }
            return $result;
        })()),
        "complete_us_internet_movie" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $r) {
                $result[] = $r['complete_us_internet_movie'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
