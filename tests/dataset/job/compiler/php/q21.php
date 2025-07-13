<?php
$company_name = [
    [
        "id" => 1,
        "name" => "ACME Film Works",
        "country_code" => "[us]"
    ],
    [
        "id" => 2,
        "name" => "Polish Warner",
        "country_code" => "[pl]"
    ]
];
$company_type = [
    [
        "id" => 1,
        "kind" => "production companies"
    ],
    ["id" => 2, "kind" => "other"]
];
$keyword = [
    ["id" => 1, "keyword" => "sequel"],
    ["id" => 2, "keyword" => "drama"]
];
$link_type = [
    ["id" => 1, "link" => "is follow up"],
    ["id" => 2, "link" => "references"]
];
$title = [
    [
        "id" => 10,
        "title" => "Western Return",
        "production_year" => 1975
    ],
    [
        "id" => 20,
        "title" => "Other Movie",
        "production_year" => 2015
    ]
];
$movie_companies = [
    [
        "movie_id" => 10,
        "company_id" => 1,
        "company_type_id" => 1,
        "note" => null
    ],
    [
        "movie_id" => 20,
        "company_id" => 2,
        "company_type_id" => 1,
        "note" => null
    ]
];
$movie_info = [
    ["movie_id" => 10, "info" => "Sweden"],
    ["movie_id" => 20, "info" => "USA"]
];
$movie_keyword = [
    ["movie_id" => 10, "keyword_id" => 1],
    ["movie_id" => 20, "keyword_id" => 2]
];
$movie_link = [
    ["movie_id" => 10, "link_type_id" => 1],
    ["movie_id" => 20, "link_type_id" => 2]
];
$allowed_countries = [
    "Sweden",
    "Norway",
    "Germany",
    "Denmark",
    "Swedish",
    "Denish",
    "Norwegian",
    "German"
];
$rows = (function() use ($allowed_countries, $company_name, $company_type, $keyword, $link_type, $movie_companies, $movie_info, $movie_keyword, $movie_link, $title) {
    $result = [];
    foreach ($company_name as $cn) {
        foreach ($movie_companies as $mc) {
            if ($mc['company_id'] == $cn['id']) {
                foreach ($company_type as $ct) {
                    if ($ct['id'] == $mc['company_type_id']) {
                        foreach ($title as $t) {
                            if ($t['id'] == $mc['movie_id']) {
                                foreach ($movie_keyword as $mk) {
                                    if ($mk['movie_id'] == $t['id']) {
                                        foreach ($keyword as $k) {
                                            if ($k['id'] == $mk['keyword_id']) {
                                                foreach ($movie_link as $ml) {
                                                    if ($ml['movie_id'] == $t['id']) {
                                                        foreach ($link_type as $lt) {
                                                            if ($lt['id'] == $ml['link_type_id']) {
                                                                foreach ($movie_info as $mi) {
                                                                    if ($mi['movie_id'] == $t['id']) {
                                                                        if ($cn['country_code'] != "[pl]" && (strpos($cn['name'], "Film") !== false || strpos($cn['name'], "Warner") !== false) && $ct['kind'] == "production companies" && $k['keyword'] == "sequel" && strpos($lt['link'], "follow") !== false && $mc['note'] == null && (in_array($mi['info'], $allowed_countries)) && $t['production_year'] >= 1950 && $t['production_year'] <= 2000) {
                                                                            $result[] = [
    "company_name" => $cn['name'],
    "link_type" => $lt['link'],
    "western_follow_up" => $t['title']
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
        "company_name" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['company_name'];
            }
            return $result;
        })()),
        "link_type" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['link_type'];
            }
            return $result;
        })()),
        "western_follow_up" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['western_follow_up'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
