<?php
$char_name = [
    ["id" => 1, "name" => "Ivan"],
    ["id" => 2, "name" => "Alex"]
];
$cast_info = [
    [
        "movie_id" => 10,
        "person_role_id" => 1,
        "role_id" => 1,
        "note" => "Soldier (voice) (uncredited)"
    ],
    [
        "movie_id" => 11,
        "person_role_id" => 2,
        "role_id" => 1,
        "note" => "(voice)"
    ]
];
$company_name = [
    ["id" => 1, "country_code" => "[ru]"],
    ["id" => 2, "country_code" => "[us]"]
];
$company_type = [["id" => 1], ["id" => 2]];
$movie_companies = [
    [
        "movie_id" => 10,
        "company_id" => 1,
        "company_type_id" => 1
    ],
    [
        "movie_id" => 11,
        "company_id" => 2,
        "company_type_id" => 1
    ]
];
$role_type = [
    ["id" => 1, "role" => "actor"],
    ["id" => 2, "role" => "director"]
];
$title = [
    [
        "id" => 10,
        "title" => "Vodka Dreams",
        "production_year" => 2006
    ],
    [
        "id" => 11,
        "title" => "Other Film",
        "production_year" => 2004
    ]
];
$matches = (function() use ($cast_info, $char_name, $company_name, $company_type, $movie_companies, $role_type, $title) {
    $result = [];
    foreach ($char_name as $chn) {
        foreach ($cast_info as $ci) {
            if ($chn['id'] == $ci['person_role_id']) {
                foreach ($role_type as $rt) {
                    if ($rt['id'] == $ci['role_id']) {
                        foreach ($title as $t) {
                            if ($t['id'] == $ci['movie_id']) {
                                foreach ($movie_companies as $mc) {
                                    if ($mc['movie_id'] == $t['id']) {
                                        foreach ($company_name as $cn) {
                                            if ($cn['id'] == $mc['company_id']) {
                                                foreach ($company_type as $ct) {
                                                    if ($ct['id'] == $mc['company_type_id']) {
                                                        if (strpos($ci['note'], "(voice)") !== false && strpos($ci['note'], "(uncredited)") !== false && $cn['country_code'] == "[ru]" && $rt['role'] == "actor" && $t['production_year'] > 2005) {
                                                            $result[] = [
    "character" => $chn['name'],
    "movie" => $t['title']
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
    return $result;
})();
$result = [
    [
        "uncredited_voiced_character" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['character'];
            }
            return $result;
        })()),
        "russian_movie" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['movie'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
