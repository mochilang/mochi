<?php
$aka_name = [
    ["person_id" => 1, "name" => "A. N. G."],
    ["person_id" => 2, "name" => "J. D."]
];
$char_name = [
    ["id" => 10, "name" => "Angel"],
    ["id" => 20, "name" => "Devil"]
];
$cast_info = [
    [
        "person_id" => 1,
        "person_role_id" => 10,
        "movie_id" => 100,
        "role_id" => 1000,
        "note" => "(voice)"
    ],
    [
        "person_id" => 2,
        "person_role_id" => 20,
        "movie_id" => 200,
        "role_id" => 1000,
        "note" => "(voice)"
    ]
];
$company_name = [
    ["id" => 100, "country_code" => "[us]"],
    ["id" => 200, "country_code" => "[gb]"]
];
$movie_companies = [
    [
        "movie_id" => 100,
        "company_id" => 100,
        "note" => "ACME Studios (USA)"
    ],
    [
        "movie_id" => 200,
        "company_id" => 200,
        "note" => "Maple Films"
    ]
];
$name = [
    [
        "id" => 1,
        "name" => "Angela Smith",
        "gender" => "f"
    ],
    [
        "id" => 2,
        "name" => "John Doe",
        "gender" => "m"
    ]
];
$role_type = [
    ["id" => 1000, "role" => "actress"],
    ["id" => 2000, "role" => "actor"]
];
$title = [
    [
        "id" => 100,
        "title" => "Famous Film",
        "production_year" => 2010
    ],
    [
        "id" => 200,
        "title" => "Old Movie",
        "production_year" => 1999
    ]
];
$matches = (function() use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) {
    $result = [];
    foreach ($aka_name as $an) {
        foreach ($name as $n) {
            if ($an['person_id'] == $n['id']) {
                foreach ($cast_info as $ci) {
                    if ($ci['person_id'] == $n['id']) {
                        foreach ($char_name as $chn) {
                            if ($chn['id'] == $ci['person_role_id']) {
                                foreach ($title as $t) {
                                    if ($t['id'] == $ci['movie_id']) {
                                        foreach ($movie_companies as $mc) {
                                            if ($mc['movie_id'] == $t['id']) {
                                                foreach ($company_name as $cn) {
                                                    if ($cn['id'] == $mc['company_id']) {
                                                        foreach ($role_type as $rt) {
                                                            if ($rt['id'] == $ci['role_id']) {
                                                                if ((in_array($ci['note'], [
    "(voice)",
    "(voice: Japanese version)",
    "(voice) (uncredited)",
    "(voice: English version)"
])) && $cn['country_code'] == "[us]" && (strpos($mc['note'], "(USA)") !== false || strpos($mc['note'], "(worldwide)") !== false) && $n['gender'] == "f" && strpos($n['name'], "Ang") !== false && $rt['role'] == "actress" && $t['production_year'] >= 2005 && $t['production_year'] <= 2015) {
                                                                    $result[] = [
    "alt" => $an['name'],
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
        }
    }
    return $result;
})();
$result = [
    [
        "alternative_name" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['alt'];
            }
            return $result;
        })()),
        "character_name" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['character'];
            }
            return $result;
        })()),
        "movie" => min((function() use ($matches) {
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
