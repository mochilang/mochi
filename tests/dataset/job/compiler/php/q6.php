<?php
function mochi_test_Q6_finds_marvel_movie_with_Robert_Downey() {
	global $actor_name, $marvel_movie, $movie_keyword, $result;
	if (!(($result == [["movie_keyword" => "marvel-cinematic-universe", "actor_name" => "Downey Robert Jr.", "marvel_movie" => "Iron Man 3"]]))) { throw new Exception('expect failed'); }
}

$cast_info = [["movie_id" => 1, "person_id" => 101], ["movie_id" => 2, "person_id" => 102]];
$keyword = [["id" => 100, "keyword" => "marvel-cinematic-universe"], ["id" => 200, "keyword" => "other"]];
$movie_keyword = [["movie_id" => 1, "keyword_id" => 100], ["movie_id" => 2, "keyword_id" => 200]];
$name = [["id" => 101, "name" => "Downey Robert Jr."], ["id" => 102, "name" => "Chris Evans"]];
$title = [["id" => 1, "title" => "Iron Man 3", "production_year" => 2013], ["id" => 2, "title" => "Old Movie", "production_year" => 2000]];
$result = (function() use ($cast_info, $keyword, $movie_keyword, $name, $title) {
	$_src = $cast_info;
	return _query($_src, [
		[ 'items' => $movie_keyword, 'on' => function($ci, $mk) use ($cast_info, $keyword, $movie_keyword, $name, $title) { return ($ci['movie_id'] == $mk['movie_id']); } ],
		[ 'items' => $keyword, 'on' => function($ci, $mk, $k) use ($cast_info, $keyword, $movie_keyword, $name, $title) { return ($mk['keyword_id'] == $k['id']); } ],
		[ 'items' => $name, 'on' => function($ci, $mk, $k, $n) use ($cast_info, $keyword, $movie_keyword, $name, $title) { return ($ci['person_id'] == $n['id']); } ],
		[ 'items' => $title, 'on' => function($ci, $mk, $k, $n, $t) use ($cast_info, $keyword, $movie_keyword, $name, $title) { return ($ci['movie_id'] == $t['id']); } ]
	], [ 'select' => function($ci, $mk, $k, $n, $t) use ($cast_info, $keyword, $movie_keyword, $name, $title) { return ["movie_keyword" => $k['keyword'], "actor_name" => $n['name'], "marvel_movie" => $t['title']]; }, 'where' => function($ci, $mk, $k, $n, $t) use ($cast_info, $keyword, $movie_keyword, $name, $title) { return ((((($k['keyword'] == "marvel-cinematic-universe") && (is_array($n['name']) ? (array_key_exists("Downey", $n['name']) || in_array("Downey", $n['name'], true)) : (is_string($n['name']) ? strpos($n['name'], strval("Downey")) !== false : false))) && (is_array($n['name']) ? (array_key_exists("Robert", $n['name']) || in_array("Robert", $n['name'], true)) : (is_string($n['name']) ? strpos($n['name'], strval("Robert")) !== false : false))) && ($t['production_year'] > 2010))); } ]);
})();
echo json_encode($result), PHP_EOL;
mochi_test_Q6_finds_marvel_movie_with_Robert_Downey();

function _query($src, $joins, $opts) {
    $items = array_map(fn($v) => [$v], $src);
    foreach ($joins as $j) {
        $joined = [];
        if (!empty($j['right']) && !empty($j['left'])) {
            $matched = array_fill(0, count($j['items']), false);
            foreach ($items as $left) {
                $m = false;
                foreach ($j['items'] as $ri => $right) {
                    $keep = true;
                    if (isset($j['on'])) { $args = array_merge($left, [$right]); $keep = $j['on'](...$args); }
                    if (!$keep) continue;
                    $m = true; $matched[$ri] = true;
                    $joined[] = array_merge($left, [$right]);
                }
                if (!$m) { $joined[] = array_merge($left, [null]); }
            }
            foreach ($j['items'] as $ri => $right) {
                if (!$matched[$ri]) {
                    $undef = count($items) > 0 ? array_fill(0, count($items[0]), null) : [];
                    $joined[] = array_merge($undef, [$right]);
                }
            }
        } elseif (!empty($j['right'])) {
            foreach ($j['items'] as $right) {
                $m = false;
                foreach ($items as $left) {
                    $keep = true;
                    if (isset($j['on'])) { $args = array_merge($left, [$right]); $keep = $j['on'](...$args); }
                    if (!$keep) continue;
                    $m = true; $joined[] = array_merge($left, [$right]);
                }
                if (!$m) {
                    $undef = count($items) > 0 ? array_fill(0, count($items[0]), null) : [];
                    $joined[] = array_merge($undef, [$right]);
                }
            }
        } else {
            foreach ($items as $left) {
                $m = false;
                foreach ($j['items'] as $right) {
                    $keep = true;
                    if (isset($j['on'])) { $args = array_merge($left, [$right]); $keep = $j['on'](...$args); }
                    if (!$keep) continue;
                    $m = true; $joined[] = array_merge($left, [$right]);
                }
                if (!empty($j['left']) && !$m) { $joined[] = array_merge($left, [null]); }
            }
        }
        $items = $joined;
    }
    if (isset($opts['where'])) {
        $filtered = [];
        foreach ($items as $r) { if ($opts['where'](...$r)) $filtered[] = $r; }
        $items = $filtered;
    }
    if (isset($opts['sortKey'])) {
        $pairs = [];
        foreach ($items as $it) { $pairs[] = ['item' => $it, 'key' => $opts['sortKey'](...$it)]; }
        usort($pairs, function($a, $b) {
            $ak = $a['key']; $bk = $b['key'];
            if (is_int($ak) && is_int($bk)) return $ak <=> $bk;
            if (is_string($ak) && is_string($bk)) return $ak <=> $bk;
            return strcmp(strval($ak), strval($bk));
        });
        $items = array_map(fn($p) => $p['item'], $pairs);
    }
    if (array_key_exists('skip', $opts)) {
        $n = $opts['skip'];
        $items = $n < count($items) ? array_slice($items, $n) : [];
    }
    if (array_key_exists('take', $opts)) {
        $n = $opts['take'];
        if ($n < count($items)) $items = array_slice($items, 0, $n);
    }
    $res = [];
    foreach ($items as $r) { $res[] = $opts['select'](...$r); }
    return $res;
}
