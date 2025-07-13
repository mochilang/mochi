<?php
$info_type = [
    ["id" => 1, "info" => "budget"],
    ["id" => 2, "info" => "votes"],
    ["id" => 3, "info" => "rating"]
];
$name = [
    [
        "id" => 1,
        "name" => "Big Tim",
        "gender" => "m"
    ],
    [
        "id" => 2,
        "name" => "Slim Tim",
        "gender" => "m"
    ],
    [
        "id" => 3,
        "name" => "Alice",
        "gender" => "f"
    ]
];
$title = [
    ["id" => 10, "title" => "Alpha"],
    ["id" => 20, "title" => "Beta"],
    ["id" => 30, "title" => "Gamma"]
];
$cast_info = [
    [
        "movie_id" => 10,
        "person_id" => 1,
        "note" => "(producer)"
    ],
    [
        "movie_id" => 20,
        "person_id" => 2,
        "note" => "(executive producer)"
    ],
    [
        "movie_id" => 30,
        "person_id" => 3,
        "note" => "(producer)"
    ]
];
$movie_info = [
    [
        "movie_id" => 10,
        "info_type_id" => 1,
        "info" => 90
    ],
    [
        "movie_id" => 20,
        "info_type_id" => 1,
        "info" => 120
    ],
    [
        "movie_id" => 30,
        "info_type_id" => 1,
        "info" => 110
    ]
];
$movie_info_idx = [
    [
        "movie_id" => 10,
        "info_type_id" => 2,
        "info" => 500
    ],
    [
        "movie_id" => 20,
        "info_type_id" => 2,
        "info" => 400
    ],
    [
        "movie_id" => 30,
        "info_type_id" => 2,
        "info" => 800
    ]
];
$rows = (function() use ($cast_info, $info_type, $movie_info, $movie_info_idx, $name, $title) {
    $result = [];
    foreach ($cast_info as $ci) {
        foreach ($name as $n) {
            if ($n['id'] == $ci['person_id']) {
                foreach ($title as $t) {
                    if ($t['id'] == $ci['movie_id']) {
                        foreach ($movie_info as $mi) {
                            if ($mi['movie_id'] == $t['id']) {
                                foreach ($movie_info_idx as $mi_idx) {
                                    if ($mi_idx['movie_id'] == $t['id']) {
                                        foreach ($info_type as $it1) {
                                            if ($it1['id'] == $mi['info_type_id']) {
                                                foreach ($info_type as $it2) {
                                                    if ($it2['id'] == $mi_idx['info_type_id']) {
                                                        if ((in_array($ci['note'], ["(producer)", "(executive producer)"]) && $it1['info'] == "budget" && $it2['info'] == "votes" && $n['gender'] == "m" && strpos($n['name'], "Tim") !== false && $t['id'] == $ci['movie_id'] && $ci['movie_id'] == $mi['movie_id'] && $ci['movie_id'] == $mi_idx['movie_id'] && $mi['movie_id'] == $mi_idx['movie_id'])) {
                                                            $result[] = [
    "budget" => $mi['info'],
    "votes" => $mi_idx['info'],
    "title" => $t['title']
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
    "movie_budget" => min((function() use ($rows) {
        $result = [];
        foreach ($rows as $r) {
            $result[] = $r['budget'];
        }
        return $result;
    })()),
    "movie_votes" => min((function() use ($rows) {
        $result = [];
        foreach ($rows as $r) {
            $result[] = $r['votes'];
        }
        return $result;
    })()),
    "movie_title" => min((function() use ($rows) {
        $result = [];
        foreach ($rows as $r) {
            $result[] = $r['title'];
        }
        return $result;
    })())
];
echo json_encode($result), PHP_EOL;
?>
