<?php
$aka_name = [["person_id" => 1], ["person_id" => 2]];
$complete_cast = [
    [
        "movie_id" => 1,
        "subject_id" => 1,
        "status_id" => 2
    ],
    [
        "movie_id" => 2,
        "subject_id" => 1,
        "status_id" => 2
    ]
];
$comp_cast_type = [
    ["id" => 1, "kind" => "cast"],
    [
        "id" => 2,
        "kind" => "complete+verified"
    ],
    ["id" => 3, "kind" => "other"]
];
$char_name = [
    ["id" => 1, "name" => "Queen"],
    ["id" => 2, "name" => "Princess"]
];
$cast_info = [
    [
        "movie_id" => 1,
        "person_id" => 1,
        "role_id" => 1,
        "person_role_id" => 1,
        "note" => "(voice)"
    ],
    [
        "movie_id" => 2,
        "person_id" => 2,
        "role_id" => 1,
        "person_role_id" => 2,
        "note" => "(voice)"
    ]
];
$company_name = [
    ["id" => 1, "country_code" => "[us]"],
    ["id" => 2, "country_code" => "[uk]"]
];
$info_type = [
    ["id" => 1, "info" => "release dates"],
    ["id" => 2, "info" => "trivia"],
    ["id" => 3, "info" => "other"]
];
$keyword = [
    [
        "id" => 1,
        "keyword" => "computer-animation"
    ],
    ["id" => 2, "keyword" => "action"]
];
$movie_companies = [
    ["movie_id" => 1, "company_id" => 1],
    ["movie_id" => 2, "company_id" => 2]
];
$movie_info = [
    [
        "movie_id" => 1,
        "info_type_id" => 1,
        "info" => "USA:2004"
    ],
    [
        "movie_id" => 2,
        "info_type_id" => 1,
        "info" => "USA:1995"
    ]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 1],
    ["movie_id" => 2, "keyword_id" => 2]
];
$name = [
    [
        "id" => 1,
        "name" => "Angela Aniston",
        "gender" => "f"
    ],
    [
        "id" => 2,
        "name" => "Bob Brown",
        "gender" => "m"
    ]
];
$person_info = [
    ["person_id" => 1, "info_type_id" => 2],
    ["person_id" => 2, "info_type_id" => 2]
];
$role_type = [
    ["id" => 1, "role" => "actress"],
    ["id" => 2, "role" => "actor"]
];
$title = [
    [
        "id" => 1,
        "title" => "Shrek 2",
        "production_year" => 2004
    ],
    [
        "id" => 2,
        "title" => "Old Film",
        "production_year" => 1999
    ]
];
$matches = (function() use ($aka_name, $cast_info, $char_name, $comp_cast_type, $company_name, $complete_cast, $info_type, $keyword, $movie_companies, $movie_info, $movie_keyword, $name, $person_info, $role_type, $title) {
    $result = [];
    foreach ($aka_name as $an) {
        foreach ($complete_cast as $cc) {
            foreach ($comp_cast_type as $cct1) {
                foreach ($comp_cast_type as $cct2) {
                    foreach ($char_name as $chn) {
                        foreach ($cast_info as $ci) {
                            foreach ($company_name as $cn) {
                                foreach ($info_type as $it) {
                                    foreach ($info_type as $it3) {
                                        foreach ($keyword as $k) {
                                            foreach ($movie_companies as $mc) {
                                                foreach ($movie_info as $mi) {
                                                    foreach ($movie_keyword as $mk) {
                                                        foreach ($name as $n) {
                                                            foreach ($person_info as $pi) {
                                                                foreach ($role_type as $rt) {
                                                                    foreach ($title as $t) {
                                                                        if (($cct1['kind'] == "cast" && $cct2['kind'] == "complete+verified" && $chn['name'] == "Queen" && ($ci['note'] == "(voice)" || $ci['note'] == "(voice) (uncredited)" || $ci['note'] == "(voice: English version)") && $cn['country_code'] == "[us]" && $it['info'] == "release dates" && $it3['info'] == "trivia" && $k['keyword'] == "computer-animation" && (str_starts_with($mi['info'], "Japan:200") || str_starts_with($mi['info'], "USA:200")) && $n['gender'] == "f" && strpos($n['name'], "An") !== false && $rt['role'] == "actress" && $t['title'] == "Shrek 2" && $t['production_year'] >= 2000 && $t['production_year'] <= 2010 && $t['id'] == $mi['movie_id'] && $t['id'] == $mc['movie_id'] && $t['id'] == $ci['movie_id'] && $t['id'] == $mk['movie_id'] && $t['id'] == $cc['movie_id'] && $mc['movie_id'] == $ci['movie_id'] && $mc['movie_id'] == $mi['movie_id'] && $mc['movie_id'] == $mk['movie_id'] && $mc['movie_id'] == $cc['movie_id'] && $mi['movie_id'] == $ci['movie_id'] && $mi['movie_id'] == $mk['movie_id'] && $mi['movie_id'] == $cc['movie_id'] && $ci['movie_id'] == $mk['movie_id'] && $ci['movie_id'] == $cc['movie_id'] && $mk['movie_id'] == $cc['movie_id'] && $cn['id'] == $mc['company_id'] && $it['id'] == $mi['info_type_id'] && $n['id'] == $ci['person_id'] && $rt['id'] == $ci['role_id'] && $n['id'] == $an['person_id'] && $ci['person_id'] == $an['person_id'] && $chn['id'] == $ci['person_role_id'] && $n['id'] == $pi['person_id'] && $ci['person_id'] == $pi['person_id'] && $it3['id'] == $pi['info_type_id'] && $k['id'] == $mk['keyword_id'] && $cct1['id'] == $cc['subject_id'] && $cct2['id'] == $cc['status_id'])) {
                                                                            $result[] = [
    "voiced_char" => $chn['name'],
    "voicing_actress" => $n['name'],
    "voiced_animation" => $t['title']
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
    [
        "voiced_char" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['voiced_char'];
            }
            return $result;
        })()),
        "voicing_actress" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['voicing_actress'];
            }
            return $result;
        })()),
        "voiced_animation" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['voiced_animation'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
