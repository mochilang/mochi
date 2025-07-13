<?php
$company_name = [
    ["id" => 1, "country_code" => "[de]"],
    ["id" => 2, "country_code" => "[us]"]
];
$keyword = [
    [
        "id" => 1,
        "keyword" => "character-name-in-title"
    ],
    ["id" => 2, "keyword" => "other"]
];
$movie_companies = [
    ["movie_id" => 100, "company_id" => 1],
    ["movie_id" => 200, "company_id" => 2]
];
$movie_keyword = [
    ["movie_id" => 100, "keyword_id" => 1],
    ["movie_id" => 200, "keyword_id" => 2]
];
$title = [
    ["id" => 100, "title" => "Der Film"],
    ["id" => 200, "title" => "Other Movie"]
];
$titles = (function() use ($company_name, $keyword, $movie_companies, $movie_keyword, $title) {
    $result = [];
    foreach ($company_name as $cn) {
        foreach ($movie_companies as $mc) {
            if ($mc['company_id'] == $cn['id']) {
                foreach ($title as $t) {
                    if ($mc['movie_id'] == $t['id']) {
                        foreach ($movie_keyword as $mk) {
                            if ($mk['movie_id'] == $t['id']) {
                                foreach ($keyword as $k) {
                                    if ($mk['keyword_id'] == $k['id']) {
                                        if ($cn['country_code'] == "[de]" && $k['keyword'] == "character-name-in-title" && $mc['movie_id'] == $mk['movie_id']) {
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
$result = min($titles);
echo json_encode($result), PHP_EOL;
?>
