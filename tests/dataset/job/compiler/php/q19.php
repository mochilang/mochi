<?php
$aka_name = [
    ["person_id" => 1, "name" => "A. Stone"],
    ["person_id" => 2, "name" => "J. Doe"]
];
$char_name = [
    ["id" => 1, "name" => "Protagonist"],
    ["id" => 2, "name" => "Extra"]
];
$cast_info = [
    [
        "movie_id" => 1,
        "person_role_id" => 1,
        "person_id" => 1,
        "role_id" => 1,
        "note" => "(voice)"
    ],
    [
        "movie_id" => 2,
        "person_role_id" => 2,
        "person_id" => 2,
        "role_id" => 2,
        "note" => "Cameo"
    ]
];
$company_name = [
    ["id" => 10, "country_code" => "[us]"],
    ["id" => 20, "country_code" => "[gb]"]
];
$info_type = [
    ["id" => 100, "info" => "release dates"]
];
$movie_companies = [
    [
        "movie_id" => 1,
        "company_id" => 10,
        "note" => "Studio (USA)"
    ],
    [
        "movie_id" => 2,
        "company_id" => 20,
        "note" => "Other (worldwide)"
    ]
];
$movie_info = [
    [
        "movie_id" => 1,
        "info_type_id" => 100,
        "info" => "USA: June 2006"
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 100,
        "info" => "UK: 1999"
    ]
];
$name = [
    [
        "id" => 1,
        "name" => "Angela Stone",
        "gender" => "f"
    ],
    [
        "id" => 2,
        "name" => "Bob Angstrom",
        "gender" => "m"
    ]
];
$role_type = [
    ["id" => 1, "role" => "actress"],
    ["id" => 2, "role" => "actor"]
];
$title = [
    [
        "id" => 1,
        "title" => "Voiced Movie",
        "production_year" => 2006
    ],
    [
        "id" => 2,
        "title" => "Other Movie",
        "production_year" => 2010
    ]
];
$matches = (function() use ($aka_name, $cast_info, $char_name, $company_name, $info_type, $movie_companies, $movie_info, $name, $role_type, $title) {
    $result = [];
    foreach ($aka_name as $an) {
        foreach ($name as $n) {
            if ($n['id'] == $an['person_id']) {
                foreach ($cast_info as $ci) {
                    if ($ci['person_id'] == $an['person_id']) {
                        foreach ($char_name as $chn) {
                            if ($chn['id'] == $ci['person_role_id']) {
                                foreach ($role_type as $rt) {
                                    if ($rt['id'] == $ci['role_id']) {
                                        foreach ($title as $t) {
                                            if ($t['id'] == $ci['movie_id']) {
                                                foreach ($movie_companies as $mc) {
                                                    if ($mc['movie_id'] == $t['id']) {
                                                        foreach ($company_name as $cn) {
                                                            if ($cn['id'] == $mc['company_id']) {
                                                                foreach ($movie_info as $mi) {
                                                                    if ($mi['movie_id'] == $t['id']) {
                                                                        foreach ($info_type as $it) {
                                                                            if ($it['id'] == $mi['info_type_id']) {
                                                                                if (in_array($ci['note'], [
    "(voice)",
    "(voice: Japanese version)",
    "(voice) (uncredited)",
    "(voice: English version)"
]) && $cn['country_code'] == "[us]" && $it['info'] == "release dates" && $mc['note'] != null && (strpos($mc['note'], "(USA)") !== false || strpos($mc['note'], "(worldwide)") !== false) && $mi['info'] != null && ((strpos($mi['info'], "Japan:") !== false && strpos($mi['info'], "200") !== false) || (strpos($mi['info'], "USA:") !== false && strpos($mi['info'], "200") !== false)) && $n['gender'] == "f" && strpos($n['name'], "Ang") !== false && $rt['role'] == "actress" && $t['production_year'] >= 2005 && $t['production_year'] <= 2009) {
                                                                                    $result[] = [
    "actress" => $n['name'],
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
                }
            }
        }
    }
    return $result;
})();
$result = [
    [
        "voicing_actress" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $r) {
                $result[] = $r['actress'];
            }
            return $result;
        })()),
        "voiced_movie" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $r) {
                $result[] = $r['movie'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
