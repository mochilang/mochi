<?php
$aka_name = [["person_id" => 1]];
$char_name = [
    ["id" => 1, "name" => "Hero Character"]
];
$cast_info = [
    [
        "movie_id" => 1,
        "person_id" => 1,
        "person_role_id" => 1,
        "role_id" => 1,
        "note" => "(voice)"
    ]
];
$company_name = [["id" => 1, "country_code" => "[us]"]];
$info_type = [["id" => 1, "info" => "release dates"]];
$keyword = [["id" => 1, "keyword" => "hero"]];
$movie_companies = [["movie_id" => 1, "company_id" => 1]];
$movie_info = [
    [
        "movie_id" => 1,
        "info_type_id" => 1,
        "info" => "Japan: Feb 2015"
    ]
];
$movie_keyword = [["movie_id" => 1, "keyword_id" => 1]];
$name = [
    [
        "id" => 1,
        "name" => "Ann Actress",
        "gender" => "f"
    ]
];
$role_type = [["id" => 1, "role" => "actress"]];
$title = [
    [
        "id" => 1,
        "title" => "Heroic Adventure",
        "production_year" => 2015
    ]
];
$matches = (function() use ($aka_name, $cast_info, $char_name, $company_name, $info_type, $keyword, $movie_companies, $movie_info, $movie_keyword, $name, $role_type, $title) {
    $result = [];
    foreach ($aka_name as $an) {
        foreach ($char_name as $chn) {
            foreach ($cast_info as $ci) {
                foreach ($company_name as $cn) {
                    foreach ($info_type as $it) {
                        foreach ($keyword as $k) {
                            foreach ($movie_companies as $mc) {
                                foreach ($movie_info as $mi) {
                                    foreach ($movie_keyword as $mk) {
                                        foreach ($name as $n) {
                                            foreach ($role_type as $rt) {
                                                foreach ($title as $t) {
                                                    if ((in_array(in_array($ci['note'], [
    "(voice)",
    "(voice: Japanese version)",
    "(voice) (uncredited)",
    "(voice: English version)"
]) && $cn['country_code'] == "[us]" && $it['info'] == "release dates" && $k['keyword'], [
    "hero",
    "martial-arts",
    "hand-to-hand-combat"
]) && $mi['info'] != null && (str_starts_with($mi['info'], "Japan:") && strpos($mi['info'], "201") !== false || str_starts_with($mi['info'], "USA:") && strpos($mi['info'], "201") !== false) && $n['gender'] == "f" && strpos($n['name'], "An") !== false && $rt['role'] == "actress" && $t['production_year'] > 2010 && $t['id'] == $mi['movie_id'] && $t['id'] == $mc['movie_id'] && $t['id'] == $ci['movie_id'] && $t['id'] == $mk['movie_id'] && $mc['movie_id'] == $ci['movie_id'] && $mc['movie_id'] == $mi['movie_id'] && $mc['movie_id'] == $mk['movie_id'] && $mi['movie_id'] == $ci['movie_id'] && $mi['movie_id'] == $mk['movie_id'] && $ci['movie_id'] == $mk['movie_id'] && $cn['id'] == $mc['company_id'] && $it['id'] == $mi['info_type_id'] && $n['id'] == $ci['person_id'] && $rt['id'] == $ci['role_id'] && $n['id'] == $an['person_id'] && $ci['person_id'] == $an['person_id'] && $chn['id'] == $ci['person_role_id'] && $k['id'] == $mk['keyword_id'])) {
                                                        $result[] = [
    "voiced_char_name" => $chn['name'],
    "voicing_actress_name" => $n['name'],
    "voiced_action_movie_jap_eng" => $t['title']
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
    return $result;
})();
$result = [
    [
        "voiced_char_name" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['voiced_char_name'];
            }
            return $result;
        })()),
        "voicing_actress_name" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['voicing_actress_name'];
            }
            return $result;
        })()),
        "voiced_action_movie_jap_eng" => min((function() use ($matches) {
            $result = [];
            foreach ($matches as $x) {
                $result[] = $x['voiced_action_movie_jap_eng'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
