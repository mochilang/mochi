<?php
$comp_cast_type = [
    ["id" => 1, "kind" => "cast"],
    ["id" => 2, "kind" => "crew"],
    ["id" => 3, "kind" => "complete"]
];
$complete_cast = [
    [
        "movie_id" => 1,
        "subject_id" => 1,
        "status_id" => 3
    ],
    [
        "movie_id" => 2,
        "subject_id" => 2,
        "status_id" => 3
    ]
];
$company_name = [
    [
        "id" => 1,
        "name" => "Best Film",
        "country_code" => "[se]"
    ],
    [
        "id" => 2,
        "name" => "Polish Film",
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
    ["id" => 2, "keyword" => "remake"]
];
$link_type = [
    ["id" => 1, "link" => "follows"],
    ["id" => 2, "link" => "related"]
];
$movie_companies = [
    [
        "movie_id" => 1,
        "company_id" => 1,
        "company_type_id" => 1,
        "note" => null
    ],
    [
        "movie_id" => 2,
        "company_id" => 2,
        "company_type_id" => 1,
        "note" => "extra"
    ]
];
$movie_info = [
    ["movie_id" => 1, "info" => "Sweden"],
    ["movie_id" => 2, "info" => "USA"]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 1],
    ["movie_id" => 2, "keyword_id" => 2]
];
$movie_link = [
    ["movie_id" => 1, "link_type_id" => 1],
    ["movie_id" => 2, "link_type_id" => 2]
];
$title = [
    [
        "id" => 1,
        "production_year" => 1980,
        "title" => "Western Sequel"
    ],
    [
        "id" => 2,
        "production_year" => 1999,
        "title" => "Another Movie"
    ]
];
$matches = (function() use ($comp_cast_type, $company_name, $company_type, $complete_cast, $keyword, $link_type, $movie_companies, $movie_info, $movie_keyword, $movie_link, $title) {
    $result = [];
    foreach ($complete_cast as $cc) {
        foreach ($comp_cast_type as $cct1) {
            if ($cct1['id'] == $cc['subject_id']) {
                foreach ($comp_cast_type as $cct2) {
                    if ($cct2['id'] == $cc['status_id']) {
                        foreach ($title as $t) {
                            if ($t['id'] == $cc['movie_id']) {
                                foreach ($movie_link as $ml) {
                                    if ($ml['movie_id'] == $t['id']) {
                                        foreach ($link_type as $lt) {
                                            if ($lt['id'] == $ml['link_type_id']) {
                                                foreach ($movie_keyword as $mk) {
                                                    if ($mk['movie_id'] == $t['id']) {
                                                        foreach ($keyword as $k) {
                                                            if ($k['id'] == $mk['keyword_id']) {
                                                                foreach ($movie_companies as $mc) {
                                                                    if ($mc['movie_id'] == $t['id']) {
                                                                        foreach ($company_type as $ct) {
                                                                            if ($ct['id'] == $mc['company_type_id']) {
                                                                                foreach ($company_name as $cn) {
                                                                                    if ($cn['id'] == $mc['company_id']) {
                                                                                        foreach ($movie_info as $mi) {
                                                                                            if ($mi['movie_id'] == $t['id']) {
                                                                                                if ((($cct1['kind'] == "cast" || $cct1['kind'] == "crew") && $cct2['kind'] == "complete" && $cn['country_code'] != "[pl]" && (strpos($cn['name'], "Film") !== false || strpos($cn['name'], "Warner") !== false) && $ct['kind'] == "production companies" && $k['keyword'] == "sequel" && strpos($lt['link'], "follow") !== false && $mc['note'] == null && ($mi['info'] == "Sweden" || $mi['info'] == "Germany" || $mi['info'] == "Swedish" || $mi['info'] == "German") && $t['production_year'] >= 1950 && $t['production_year'] <= 2000 && $ml['movie_id'] == $mk['movie_id'] && $ml['movie_id'] == $mc['movie_id'] && $mk['movie_id'] == $mc['movie_id'] && $ml['movie_id'] == $mi['movie_id'] && $mk['movie_id'] == $mi['movie_id'] && $mc['movie_id'] == $mi['movie_id'] && $ml['movie_id'] == $cc['movie_id'] && $mk['movie_id'] == $cc['movie_id'] && $mc['movie_id'] == $cc['movie_id'] && $mi['movie_id'] == $cc['movie_id'])) {
                                                                                                    $result[] = [
    "company" => $cn['name'],
    "link" => $lt['link'],
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
    return $result;
})();
$result = [
    "producing_company" => min((function() use ($matches) {
        $result = [];
        foreach ($matches as $x) {
            $result[] = $x['company'];
        }
        return $result;
    })()),
    "link_type" => min((function() use ($matches) {
        $result = [];
        foreach ($matches as $x) {
            $result[] = $x['link'];
        }
        return $result;
    })()),
    "complete_western_sequel" => min((function() use ($matches) {
        $result = [];
        foreach ($matches as $x) {
            $result[] = $x['title'];
        }
        return $result;
    })())
];
echo json_encode($result), PHP_EOL;
?>
