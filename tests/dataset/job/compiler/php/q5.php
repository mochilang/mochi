<?php
$company_type = [
    [
        "ct_id" => 1,
        "kind" => "production companies"
    ],
    ["ct_id" => 2, "kind" => "other"]
];
$info_type = [["it_id" => 10, "info" => "languages"]];
$title = [
    [
        "t_id" => 100,
        "title" => "B Movie",
        "production_year" => 2010
    ],
    [
        "t_id" => 200,
        "title" => "A Film",
        "production_year" => 2012
    ],
    [
        "t_id" => 300,
        "title" => "Old Movie",
        "production_year" => 2000
    ]
];
$movie_companies = [
    [
        "movie_id" => 100,
        "company_type_id" => 1,
        "note" => "ACME (France) (theatrical)"
    ],
    [
        "movie_id" => 200,
        "company_type_id" => 1,
        "note" => "ACME (France) (theatrical)"
    ],
    [
        "movie_id" => 300,
        "company_type_id" => 1,
        "note" => "ACME (France) (theatrical)"
    ]
];
$movie_info = [
    [
        "movie_id" => 100,
        "info" => "German",
        "info_type_id" => 10
    ],
    [
        "movie_id" => 200,
        "info" => "Swedish",
        "info_type_id" => 10
    ],
    [
        "movie_id" => 300,
        "info" => "German",
        "info_type_id" => 10
    ]
];
$candidate_titles = (function() use ($company_type, $info_type, $movie_companies, $movie_info, $title) {
    $result = [];
    foreach ($company_type as $ct) {
        foreach ($movie_companies as $mc) {
            if ($mc['company_type_id'] == $ct['ct_id']) {
                foreach ($movie_info as $mi) {
                    if ($mi['movie_id'] == $mc['movie_id']) {
                        foreach ($info_type as $it) {
                            if ($it['it_id'] == $mi['info_type_id']) {
                                foreach ($title as $t) {
                                    if ($t['t_id'] == $mc['movie_id']) {
                                        if ((is_array($mc['note']) ? in_array((is_array($mc['note']) ? in_array($ct['kind'] == "production companies" && "(theatrical)", $mc['note'], true) : (is_string($mc['note']) ? strpos($mc['note'], strval($ct['kind'] == "production companies" && "(theatrical)")) !== false : false)) && "(France)", $mc['note'], true) : (is_string($mc['note']) ? strpos($mc['note'], strval((is_array($mc['note']) ? in_array($ct['kind'] == "production companies" && "(theatrical)", $mc['note'], true) : (is_string($mc['note']) ? strpos($mc['note'], strval($ct['kind'] == "production companies" && "(theatrical)")) !== false : false)) && "(France)")) !== false : false)) && $t['production_year'] > 2005 && (in_array($mi['info'], [
    "Sweden",
    "Norway",
    "Germany",
    "Denmark",
    "Swedish",
    "Denish",
    "Norwegian",
    "German"
]))) {
                                            $result[] = $t['title'];
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
        "typical_european_movie" => min($candidate_titles)
    ]
];
echo json_encode($result), PHP_EOL;
?>
