<?php
$company_name = [
    ["id" => 1, "country_code" => "[de]"],
    ["id" => 2, "country_code" => "[us]"]
];
$company_type = [
    [
        "id" => 1,
        "kind" => "production companies"
    ],
    ["id" => 2, "kind" => "distributors"]
];
$info_type = [
    ["id" => 1, "info" => "rating"],
    ["id" => 2, "info" => "release dates"]
];
$kind_type = [
    ["id" => 1, "kind" => "movie"],
    ["id" => 2, "kind" => "video"]
];
$title = [
    [
        "id" => 10,
        "kind_id" => 1,
        "title" => "Alpha"
    ],
    [
        "id" => 20,
        "kind_id" => 1,
        "title" => "Beta"
    ],
    [
        "id" => 30,
        "kind_id" => 2,
        "title" => "Gamma"
    ]
];
$movie_companies = [
    [
        "movie_id" => 10,
        "company_id" => 1,
        "company_type_id" => 1
    ],
    [
        "movie_id" => 20,
        "company_id" => 1,
        "company_type_id" => 1
    ],
    [
        "movie_id" => 30,
        "company_id" => 2,
        "company_type_id" => 1
    ]
];
$movie_info = [
    [
        "movie_id" => 10,
        "info_type_id" => 2,
        "info" => "1997-05-10"
    ],
    [
        "movie_id" => 20,
        "info_type_id" => 2,
        "info" => "1998-03-20"
    ],
    [
        "movie_id" => 30,
        "info_type_id" => 2,
        "info" => "1999-07-30"
    ]
];
$movie_info_idx = [
    [
        "movie_id" => 10,
        "info_type_id" => 1,
        "info" => "6.0"
    ],
    [
        "movie_id" => 20,
        "info_type_id" => 1,
        "info" => "7.5"
    ],
    [
        "movie_id" => 30,
        "info_type_id" => 1,
        "info" => "5.5"
    ]
];
$candidates = (function() use ($company_name, $company_type, $info_type, $kind_type, $movie_companies, $movie_info, $movie_info_idx, $title) {
    $result = [];
    foreach ($company_name as $cn) {
        foreach ($movie_companies as $mc) {
            if ($mc['company_id'] == $cn['id']) {
                foreach ($company_type as $ct) {
                    if ($ct['id'] == $mc['company_type_id']) {
                        foreach ($title as $t) {
                            if ($t['id'] == $mc['movie_id']) {
                                foreach ($kind_type as $kt) {
                                    if ($kt['id'] == $t['kind_id']) {
                                        foreach ($movie_info as $mi) {
                                            if ($mi['movie_id'] == $t['id']) {
                                                foreach ($info_type as $it2) {
                                                    if ($it2['id'] == $mi['info_type_id']) {
                                                        foreach ($movie_info_idx as $miidx) {
                                                            if ($miidx['movie_id'] == $t['id']) {
                                                                foreach ($info_type as $it) {
                                                                    if ($it['id'] == $miidx['info_type_id']) {
                                                                        if ($cn['country_code'] == "[de]" && $ct['kind'] == "production companies" && $it['info'] == "rating" && $it2['info'] == "release dates" && $kt['kind'] == "movie") {
                                                                            $result[] = [
    "release_date" => $mi['info'],
    "rating" => $miidx['info'],
    "german_movie" => $t['title']
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
    "release_date" => ((function() use ($candidates) {
        $result = [];
        foreach ($candidates as $x) {
            $result[] = [$x['release_date'], $x['release_date']];
        }
        usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
        $result = array_map(fn($r) => $r[1], $result);
        return $result;
    })())[0],
    "rating" => ((function() use ($candidates) {
        $result = [];
        foreach ($candidates as $x) {
            $result[] = [$x['rating'], $x['rating']];
        }
        usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
        $result = array_map(fn($r) => $r[1], $result);
        return $result;
    })())[0],
    "german_movie" => ((function() use ($candidates) {
        $result = [];
        foreach ($candidates as $x) {
            $result[] = [$x['german_movie'], $x['german_movie']];
        }
        usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
        $result = array_map(fn($r) => $r[1], $result);
        return $result;
    })())[0]
];
echo json_encode($result), PHP_EOL;
?>
