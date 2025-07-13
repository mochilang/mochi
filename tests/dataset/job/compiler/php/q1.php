<?php
$company_type = [
    [
        "id" => 1,
        "kind" => "production companies"
    ],
    ["id" => 2, "kind" => "distributors"]
];
$info_type = [
    ["id" => 10, "info" => "top 250 rank"],
    ["id" => 20, "info" => "bottom 10 rank"]
];
$title = [
    [
        "id" => 100,
        "title" => "Good Movie",
        "production_year" => 1995
    ],
    [
        "id" => 200,
        "title" => "Bad Movie",
        "production_year" => 2000
    ]
];
$movie_companies = [
    [
        "movie_id" => 100,
        "company_type_id" => 1,
        "note" => "ACME (co-production)"
    ],
    [
        "movie_id" => 200,
        "company_type_id" => 1,
        "note" => "MGM (as Metro-Goldwyn-Mayer Pictures)"
    ]
];
$movie_info_idx = [
    [
        "movie_id" => 100,
        "info_type_id" => 10
    ],
    [
        "movie_id" => 200,
        "info_type_id" => 20
    ]
];
$filtered = (function() use ($company_type, $info_type, $movie_companies, $movie_info_idx, $title) {
    $result = [];
    foreach ($company_type as $ct) {
        foreach ($movie_companies as $mc) {
            if ($ct['id'] == $mc['company_type_id']) {
                foreach ($title as $t) {
                    if ($t['id'] == $mc['movie_id']) {
                        foreach ($movie_info_idx as $mi) {
                            if ($mi['movie_id'] == $t['id']) {
                                foreach ($info_type as $it) {
                                    if ($it['id'] == $mi['info_type_id']) {
                                        if ($ct['kind'] == "production companies" && $it['info'] == "top 250 rank" && (!strpos($mc['note'], "(as Metro-Goldwyn-Mayer Pictures)") !== false) && (strpos($mc['note'], "(co-production)") !== false || strpos($mc['note'], "(presents)") !== false)) {
                                            $result[] = [
    "note" => $mc['note'],
    "title" => $t['title'],
    "year" => $t['production_year']
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
    return $result;
})();
$result = [
    "production_note" => min((function() use ($filtered) {
        $result = [];
        foreach ($filtered as $r) {
            $result[] = $r['note'];
        }
        return $result;
    })()),
    "movie_title" => min((function() use ($filtered) {
        $result = [];
        foreach ($filtered as $r) {
            $result[] = $r['title'];
        }
        return $result;
    })()),
    "movie_year" => min((function() use ($filtered) {
        $result = [];
        foreach ($filtered as $r) {
            $result[] = $r['year'];
        }
        return $result;
    })())
];
echo json_encode([$result]), PHP_EOL;
?>
