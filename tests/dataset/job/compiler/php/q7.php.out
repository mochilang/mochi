<?php
function mochi_test_Q7_finds_movie_features_biography_for_person() {
	global $biography_movie, $of_person, $result;
	if (!(($result == [["of_person" => "Alan Brown", "biography_movie" => "Feature Film"]]))) { throw new Exception('expect failed'); }
}

$aka_name = [["person_id" => 1, "name" => "Anna Mae"], ["person_id" => 2, "name" => "Chris"]];
$cast_info = [["person_id" => 1, "movie_id" => 10], ["person_id" => 2, "movie_id" => 20]];
$info_type = [["id" => 1, "info" => "mini biography"], ["id" => 2, "info" => "trivia"]];
$link_type = [["id" => 1, "link" => "features"], ["id" => 2, "link" => "references"]];
$movie_link = [["linked_movie_id" => 10, "link_type_id" => 1], ["linked_movie_id" => 20, "link_type_id" => 2]];
$name = [["id" => 1, "name" => "Alan Brown", "name_pcode_cf" => "B", "gender" => "m"], ["id" => 2, "name" => "Zoe", "name_pcode_cf" => "Z", "gender" => "f"]];
$person_info = [["person_id" => 1, "info_type_id" => 1, "note" => "Volker Boehm"], ["person_id" => 2, "info_type_id" => 1, "note" => "Other"]];
$title = [["id" => 10, "title" => "Feature Film", "production_year" => 1990], ["id" => 20, "title" => "Late Film", "production_year" => 2000]];
$rows = (function() use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) {
	$_src = $aka_name;
	return _query($_src, [
		[ 'items' => $name, 'on' => function($an, $n) use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) { return ($n['id'] == $an['person_id']); } ],
		[ 'items' => $person_info, 'on' => function($an, $n, $pi) use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) { return ($pi['person_id'] == $an['person_id']); } ],
		[ 'items' => $info_type, 'on' => function($an, $n, $pi, $it) use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) { return ($it['id'] == $pi['info_type_id']); } ],
		[ 'items' => $cast_info, 'on' => function($an, $n, $pi, $it, $ci) use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) { return ($ci['person_id'] == $n['id']); } ],
		[ 'items' => $title, 'on' => function($an, $n, $pi, $it, $ci, $t) use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) { return ($t['id'] == $ci['movie_id']); } ],
		[ 'items' => $movie_link, 'on' => function($an, $n, $pi, $it, $ci, $t, $ml) use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) { return ($ml['linked_movie_id'] == $t['id']); } ],
		[ 'items' => $link_type, 'on' => function($an, $n, $pi, $it, $ci, $t, $ml, $lt) use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) { return ($lt['id'] == $ml['link_type_id']); } ]
	], [ 'select' => function($an, $n, $pi, $it, $ci, $t, $ml, $lt) use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) { return ["person_name" => $n['name'], "movie_title" => $t['title']]; }, 'where' => function($an, $n, $pi, $it, $ci, $t, $ml, $lt) use ($aka_name, $cast_info, $info_type, $link_type, $movie_link, $name, $person_info, $title) { return (((((((((((((((is_array($an['name']) ? (array_key_exists("a", $an['name']) || in_array("a", $an['name'], true)) : (is_string($an['name']) ? strpos($an['name'], strval("a")) !== false : false)) && ($it['info'] == "mini biography")) && ($lt['link'] == "features")) && ($n['name_pcode_cf'] >= "A")) && ($n['name_pcode_cf'] <= "F")) && ((($n['gender'] == "m") || ((($n['gender'] == "f") && $n['name']['starts_with']("B")))))) && ($pi['note'] == "Volker Boehm")) && ($t['production_year'] >= 1980)) && ($t['production_year'] <= 1995)) && ($pi['person_id'] == $an['person_id'])) && ($pi['person_id'] == $ci['person_id'])) && ($an['person_id'] == $ci['person_id'])) && ($ci['movie_id'] == $ml['linked_movie_id'])))); } ]);
})();
$result = [["of_person" => min((function() use ($rows) {
	$res = [];
	foreach ((is_string($rows) ? str_split($rows) : $rows) as $r) {
		$res[] = $r['person_name'];
	}
	return $res;
})()), "biography_movie" => min((function() use ($rows) {
	$res = [];
	foreach ((is_string($rows) ? str_split($rows) : $rows) as $r) {
		$res[] = $r['movie_title'];
	}
	return $res;
})())]];
echo json_encode($result), PHP_EOL;
mochi_test_Q7_finds_movie_features_biography_for_person();

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
