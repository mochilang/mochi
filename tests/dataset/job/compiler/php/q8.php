<?php
$aka_name = [["person_id" => 1, "name" => "Y. S."]];
$cast_info = [
    [
        "person_id" => 1,
        "movie_id" => 10,
        "note" => "(voice: English version)",
        "role_id" => 1000
    ]
];
$company_name = [["id" => 50, "country_code" => "[jp]"]];
$movie_companies = [
    [
        "movie_id" => 10,
        "company_id" => 50,
        "note" => "Studio (Japan)"
    ]
];
$name = [
    ["id" => 1, "name" => "Yoko Ono"],
    ["id" => 2, "name" => "Yuichi"]
];
$role_type = [["id" => 1000, "role" => "actress"]];
$title = [["id" => 10, "title" => "Dubbed Film"]];
$eligible = (function() use ($aka_name, $cast_info, $company_name, $movie_companies, $name, $role_type, $title) {
    $result = [];
    foreach ($aka_name as $an1) {
        foreach ($name as $n1) {
            if ($n1['id'] == $an1['person_id']) {
                foreach ($cast_info as $ci) {
                    if ($ci['person_id'] == $an1['person_id']) {
                        foreach ($title as $t) {
                            if ($t['id'] == $ci['movie_id']) {
                                foreach ($movie_companies as $mc) {
                                    if ($mc['movie_id'] == $ci['movie_id']) {
                                        foreach ($company_name as $cn) {
                                            if ($cn['id'] == $mc['company_id']) {
                                                foreach ($role_type as $rt) {
                                                    if ($rt['id'] == $ci['role_id']) {
                                                        if ($ci['note'] == "(voice: English version)" && $cn['country_code'] == "[jp]" && strpos($mc['note'], "(Japan)") !== false && (!strpos($mc['note'], "(USA)") !== false) && strpos($n1['name'], "Yo") !== false && (!strpos($n1['name'], "Yu") !== false) && $rt['role'] == "actress") {
                                                            $result[] = [
    "pseudonym" => $an1['name'],
    "movie_title" => $t['title']
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
        "actress_pseudonym" => min((function() use ($eligible) {
            $result = [];
            foreach ($eligible as $x) {
                $result[] = $x['pseudonym'];
            }
            return $result;
        })()),
        "japanese_movie_dubbed" => min((function() use ($eligible) {
            $result = [];
            foreach ($eligible as $x) {
                $result[] = $x['movie_title'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
