<?php
$aka_name = [
    ["person_id" => 1, "name" => "Anna Mae"],
    ["person_id" => 2, "name" => "Chris"]
];
$cast_info = [
    ["person_id" => 1, "movie_id" => 10],
    ["person_id" => 2, "movie_id" => 20]
];
$info_type = [
    ["id" => 1, "info" => "mini biography"],
    ["id" => 2, "info" => "trivia"]
];
$link_type = [
    ["id" => 1, "link" => "features"],
    ["id" => 2, "link" => "references"]
];
$movie_link = [
    [
        "linked_movie_id" => 10,
        "link_type_id" => 1
    ],
    [
        "linked_movie_id" => 20,
        "link_type_id" => 2
    ]
];
$name = [
    [
        "id" => 1,
        "name" => "Alan Brown",
        "name_pcode_cf" => "B",
        "gender" => "m"
    ],
    [
        "id" => 2,
        "name" => "Zoe",
        "name_pcode_cf" => "Z",
        "gender" => "f"
    ]
];
$person_info = [
    [
        "person_id" => 1,
        "info_type_id" => 1,
        "note" => "Volker Boehm"
    ],
    [
        "person_id" => 2,
        "info_type_id" => 1,
        "note" => "Other"
    ]
];
$title = [
    [
        "id" => 10,
        "title" => "Feature Film",
        "production_year" => 1990
    ],
    [
        "id" => 20,
        "title" => "Late Film",
        "production_year" => 2000
    ]
];
$rows = (function() use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) {
    $result = [];
    foreach ($aka_name as $an) {
        foreach ($name as $n) {
            if ($n['id'] == $an['person_id']) {
                foreach ($person_info as $pi) {
                    if ($pi['person_id'] == $an['person_id']) {
                        foreach ($info_type as $it) {
                            if ($it['id'] == $pi['info_type_id']) {
                                foreach ($cast_info as $ci) {
                                    if ($ci['person_id'] == $n['id']) {
                                        foreach ($title as $t) {
                                            if ($t['id'] == $ci['movie_id']) {
                                                foreach ($movie_link as $ml) {
                                                    if ($ml['linked_movie_id'] == $t['id']) {
                                                        foreach ($link_type as $lt) {
                                                            if ($lt['id'] == $ml['link_type_id']) {
                                                                if ((strpos($an['name'], "a") !== false && $it['info'] == "mini biography" && $lt['link'] == "features" && $n['name_pcode_cf'] >= "A" && $n['name_pcode_cf'] <= "F" && ($n['gender'] == "m" || ($n['gender'] == "f" && $n['name']['starts_with']("B"))) && $pi['note'] == "Volker Boehm" && $t['production_year'] >= 1980 && $t['production_year'] <= 1995 && $pi['person_id'] == $an['person_id'] && $pi['person_id'] == $ci['person_id'] && $an['person_id'] == $ci['person_id'] && $ci['movie_id'] == $ml['linked_movie_id'])) {
                                                                    $result[] = [
    "person_name" => $n['name'],
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
        }
    }
    return $result;
})();
$result = [
    [
        "of_person" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['person_name'];
            }
            return $result;
        })()),
        "biography_movie" => min((function() use ($rows) {
            $result = [];
            foreach ($rows as $r) {
                $result[] = $r['movie_title'];
            }
            return $result;
        })())
    ]
];
echo json_encode($result), PHP_EOL;
?>
