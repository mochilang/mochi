<?php
$company_name = [
    [
        "id" => 1,
        "name" => "Best Pictures",
        "country_code" => "[us]"
    ],
    [
        "id" => 2,
        "name" => "Foreign Films",
        "country_code" => "[uk]"
    ]
];
$company_type = [
    [
        "id" => 10,
        "kind" => "production companies"
    ],
    ["id" => 20, "kind" => "distributors"]
];
$info_type = [
    ["id" => 100, "info" => "genres"],
    ["id" => 200, "info" => "rating"]
];
$movie_companies = [
    [
        "movie_id" => 1000,
        "company_id" => 1,
        "company_type_id" => 10
    ],
    [
        "movie_id" => 2000,
        "company_id" => 2,
        "company_type_id" => 10
    ]
];
$movie_info = [
    [
        "movie_id" => 1000,
        "info_type_id" => 100,
        "info" => "Drama"
    ],
    [
        "movie_id" => 2000,
        "info_type_id" => 100,
        "info" => "Horror"
    ]
];
$movie_info_idx = [
    [
        "movie_id" => 1000,
        "info_type_id" => 200,
        "info" => 8.3
    ],
    [
        "movie_id" => 2000,
        "info_type_id" => 200,
        "info" => 7.5
    ]
];
$title = [
    [
        "id" => 1000,
        "production_year" => 2006,
        "title" => "Great Drama"
    ],
    [
        "id" => 2000,
        "production_year" => 2007,
        "title" => "Low Rated"
    ]
];
$result = (function() use ($company_name, $company_type, $info_type, $movie_companies, $movie_info, $movie_info_idx, $title) {
    $result = [];
    foreach ($company_name as $cn) {
        foreach ($movie_companies as $mc) {
            if ($mc['company_id'] == $cn['id']) {
                foreach ($company_type as $ct) {
                    if ($ct['id'] == $mc['company_type_id']) {
                        foreach ($title as $t) {
                            if ($t['id'] == $mc['movie_id']) {
                                foreach ($movie_info as $mi) {
                                    if ($mi['movie_id'] == $t['id']) {
                                        foreach ($info_type as $it1) {
                                            if ($it1['id'] == $mi['info_type_id']) {
                                                foreach ($movie_info_idx as $mi_idx) {
                                                    if ($mi_idx['movie_id'] == $t['id']) {
                                                        foreach ($info_type as $it2) {
                                                            if ($it2['id'] == $mi_idx['info_type_id']) {
                                                                if ($cn['country_code'] == "[us]" && $ct['kind'] == "production companies" && $it1['info'] == "genres" && $it2['info'] == "rating" && ($mi['info'] == "Drama" || $mi['info'] == "Horror") && $mi_idx['info'] > 8 && $t['production_year'] >= 2005 && $t['production_year'] <= 2008) {
                                                                    $result[] = [
    "movie_company" => $cn['name'],
    "rating" => $mi_idx['info'],
    "drama_horror_movie" => $t['title']
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
echo json_encode($result), PHP_EOL;
?>
