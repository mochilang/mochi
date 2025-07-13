<?php
$cast_info = [
    ["movie_id" => 1, "person_id" => 1],
    ["movie_id" => 2, "person_id" => 2]
];
$company_name = [
    ["id" => 1, "country_code" => "[us]"],
    ["id" => 2, "country_code" => "[ca]"]
];
$keyword = [
    [
        "id" => 10,
        "keyword" => "character-name-in-title"
    ],
    ["id" => 20, "keyword" => "other"]
];
$movie_companies = [
    ["movie_id" => 1, "company_id" => 1],
    ["movie_id" => 2, "company_id" => 2]
];
$movie_keyword = [
    ["movie_id" => 1, "keyword_id" => 10],
    ["movie_id" => 2, "keyword_id" => 20]
];
$name = [
    ["id" => 1, "name" => "Bob Smith"],
    ["id" => 2, "name" => "Alice Jones"]
];
$title = [
    ["id" => 1, "title" => "Bob's Journey"],
    ["id" => 2, "title" => "Foreign Film"]
];
$matches = (function() use ($cast_info, $company_name, $keyword, $movie_companies, $movie_keyword, $name, $title) {
    $result = [];
    foreach ($name as $n) {
        foreach ($cast_info as $ci) {
            if ($ci['person_id'] == $n['id']) {
                foreach ($title as $t) {
                    if ($t['id'] == $ci['movie_id']) {
                        foreach ($movie_keyword as $mk) {
                            if ($mk['movie_id'] == $t['id']) {
                                foreach ($keyword as $k) {
                                    if ($k['id'] == $mk['keyword_id']) {
                                        foreach ($movie_companies as $mc) {
                                            if ($mc['movie_id'] == $t['id']) {
                                                foreach ($company_name as $cn) {
                                                    if ($cn['id'] == $mc['company_id']) {
                                                        if ($cn['country_code'] == "[us]" && $k['keyword'] == "character-name-in-title" && str_starts_with($n['name'], "B") && $ci['movie_id'] == $mk['movie_id'] && $ci['movie_id'] == $mc['movie_id'] && $mc['movie_id'] == $mk['movie_id']) {
                                                            $result[] = $n['name'];
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
        "member_in_charnamed_american_movie" => min($matches),
        "a1" => min($matches)
    ]
];
echo json_encode($result), PHP_EOL;
?>
