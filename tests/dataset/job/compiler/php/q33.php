<?php
$company_name = [
    [
        "id" => 1,
        "name" => "US Studio",
        "country_code" => "[us]"
    ],
    [
        "id" => 2,
        "name" => "GB Studio",
        "country_code" => "[gb]"
    ]
];
$info_type = [
    ["id" => 1, "info" => "rating"],
    ["id" => 2, "info" => "other"]
];
$kind_type = [
    ["id" => 1, "kind" => "tv series"],
    ["id" => 2, "kind" => "movie"]
];
$link_type = [
    ["id" => 1, "link" => "follows"],
    ["id" => 2, "link" => "remake of"]
];
$movie_companies = [
    ["movie_id" => 10, "company_id" => 1],
    ["movie_id" => 20, "company_id" => 2]
];
$movie_info_idx = [
    [
        "movie_id" => 10,
        "info_type_id" => 1,
        "info" => "7.0"
    ],
    [
        "movie_id" => 20,
        "info_type_id" => 1,
        "info" => "2.5"
    ]
];
$movie_link = [
    [
        "movie_id" => 10,
        "linked_movie_id" => 20,
        "link_type_id" => 1
    ]
];
$title = [
    [
        "id" => 10,
        "title" => "Series A",
        "kind_id" => 1,
        "production_year" => 2004
    ],
    [
        "id" => 20,
        "title" => "Series B",
        "kind_id" => 1,
        "production_year" => 2006
    ]
];
$rows = (function() use ($company_name, $info_type, $kind_type, $link_type, $movie_companies, $movie_info_idx, $movie_link, $title) {
    $result = [];
    foreach ($company_name as $cn1) {
        foreach ($movie_companies as $mc1) {
            if ($cn1['id'] == $mc1['company_id']) {
                foreach ($title as $t1) {
                    if ($t1['id'] == $mc1['movie_id']) {
                        foreach ($movie_info_idx as $mi_idx1) {
                            if ($mi_idx1['movie_id'] == $t1['id']) {
                                foreach ($info_type as $it1) {
                                    if ($it1['id'] == $mi_idx1['info_type_id']) {
                                        foreach ($kind_type as $kt1) {
                                            if ($kt1['id'] == $t1['kind_id']) {
                                                foreach ($movie_link as $ml) {
                                                    if ($ml['movie_id'] == $t1['id']) {
                                                        foreach ($title as $t2) {
                                                            if ($t2['id'] == $ml['linked_movie_id']) {
                                                                foreach ($movie_info_idx as $mi_idx2) {
                                                                    if ($mi_idx2['movie_id'] == $t2['id']) {
                                                                        foreach ($info_type as $it2) {
                                                                            if ($it2['id'] == $mi_idx2['info_type_id']) {
                                                                                foreach ($kind_type as $kt2) {
                                                                                    if ($kt2['id'] == $t2['kind_id']) {
                                                                                        foreach ($movie_companies as $mc2) {
                                                                                            if ($mc2['movie_id'] == $t2['id']) {
                                                                                                foreach ($company_name as $cn2) {
                                                                                                    if ($cn2['id'] == $mc2['company_id']) {
                                                                                                        foreach ($link_type as $lt) {
                                                                                                            if ($lt['id'] == $ml['link_type_id']) {
                                                                                                                if ($cn1['country_code'] == "[us]" && $it1['info'] == "rating" && $it2['info'] == "rating" && $kt1['kind'] == "tv series" && $kt2['kind'] == "tv series" && ($lt['link'] == "sequel" || $lt['link'] == "follows" || $lt['link'] == "followed by") && $mi_idx2['info'] < "3.0" && $t2['production_year'] >= 2005 && $t2['production_year'] <= 2008) {
                                                                                                                    $result[] = [
    "first_company" => $cn1['name'],
    "second_company" => $cn2['name'],
    "first_rating" => $mi_idx1['info'],
    "second_rating" => $mi_idx2['info'],
    "first_movie" => $t1['title'],
    "second_movie" => $t2['title']
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
    [
        "first_company" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['first_company'];
            }
            return $result;
        })()),
        "second_company" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['second_company'];
            }
            return $result;
        })()),
        "first_rating" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['first_rating'];
            }
            return $result;
        })()),
        "second_rating" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['second_rating'];
            }
            return $result;
        })()),
        "first_movie" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['first_movie'];
            }
            return $result;
        })()),
        "second_movie" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['second_movie'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
