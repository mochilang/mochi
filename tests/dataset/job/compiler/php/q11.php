<?php
$company_name = [
    [
        "id" => 1,
        "name" => "Best Film Co",
        "country_code" => "[us]"
    ],
    [
        "id" => 2,
        "name" => "Warner Studios",
        "country_code" => "[de]"
    ],
    [
        "id" => 3,
        "name" => "Polish Films",
        "country_code" => "[pl]"
    ]
];
$company_type = [
    [
        "id" => 1,
        "kind" => "production companies"
    ],
    ["id" => 2, "kind" => "distributors"]
];
$keyword = [
    ["id" => 1, "keyword" => "sequel"],
    ["id" => 2, "keyword" => "thriller"]
];
$link_type = [
    ["id" => 1, "link" => "follow-up"],
    ["id" => 2, "link" => "follows from"],
    ["id" => 3, "link" => "remake"]
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
    ],
    [
        "movie_id" => 30,
        "company_id" => 3,
        "company_type_id" => 1,
        "note" => null
    ]
];
$movie_keyword = [
    ["movie_id" => 10, "keyword_id" => 1],
    ["movie_id" => 20, "keyword_id" => 1],
    ["movie_id" => 20, "keyword_id" => 2],
    ["movie_id" => 30, "keyword_id" => 1]
];
$movie_link = [
    ["movie_id" => 10, "link_type_id" => 1],
    ["movie_id" => 20, "link_type_id" => 2],
    ["movie_id" => 30, "link_type_id" => 3]
];
$title = [
    [
        "id" => 10,
        "production_year" => 1960,
        "title" => "Alpha"
    ],
    [
        "id" => 20,
        "production_year" => 1970,
        "title" => "Beta"
    ],
    [
        "id" => 30,
        "production_year" => 1985,
        "title" => "Polish Movie"
    ]
];
$matches = (function() use ($company_name, $company_type, $keyword, $link_type, $movie_companies, $movie_keyword, $movie_link, $title) {
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
                                                                if ($cn['country_code'] != "[pl]" && (strpos($cn['name'], "Film") !== false || strpos($cn['name'], "Warner") !== false) && $ct['kind'] == "production companies" && $k['keyword'] == "sequel" && strpos($lt['link'], "follow") !== false && $mc['note'] == null && $t['production_year'] >= 1950 && $t['production_year'] <= 2000 && $ml['movie_id'] == $mk['movie_id'] && $ml['movie_id'] == $mc['movie_id'] && $mk['movie_id'] == $mc['movie_id']) {
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
    return $result;
})();
$result = [
    [
        "from_company" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['company'];
            }
            return $result;
        })()),
        "movie_link_type" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['link'];
            }
            return $result;
        })()),
        "non_polish_sequel_movie" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['title'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
