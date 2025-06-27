<?php
function mochi_test_Q10_finds_uncredited_voice_actor_in_Russian_movie() {
	global $result, $russian_movie, $uncredited_voiced_character;
	if (!(($result == [["uncredited_voiced_character" => "Ivan", "russian_movie" => "Vodka Dreams"]]))) { throw new Exception('expect failed'); }
}

$char_name = [["id" => 1, "name" => "Ivan"], ["id" => 2, "name" => "Alex"]];
$cast_info = [["movie_id" => 10, "person_role_id" => 1, "role_id" => 1, "note" => "Soldier (voice) (uncredited)"], ["movie_id" => 11, "person_role_id" => 2, "role_id" => 1, "note" => "(voice)"]];
$company_name = [["id" => 1, "country_code" => "[ru]"], ["id" => 2, "country_code" => "[us]"]];
$company_type = [["id" => 1], ["id" => 2]];
$movie_companies = [["movie_id" => 10, "company_id" => 1, "company_type_id" => 1], ["movie_id" => 11, "company_id" => 2, "company_type_id" => 1]];
$role_type = [["id" => 1, "role" => "actor"], ["id" => 2, "role" => "director"]];
$title = [["id" => 10, "title" => "Vodka Dreams", "production_year" => 2006], ["id" => 11, "title" => "Other Film", "production_year" => 2004]];
$matches = (function() use ($cast_info, $char_name, $company_name, $company_type, $movie_companies, $role_type, $title) {
	$_src = $char_name;
	return _query($_src, [
		[ 'items' => $cast_info, 'on' => function($chn, $ci) use ($cast_info, $char_name, $company_name, $company_type, $movie_companies, $role_type, $title) { return ($chn['id'] == $ci['person_role_id']); } ],
		[ 'items' => $role_type, 'on' => function($chn, $ci, $rt) use ($cast_info, $char_name, $company_name, $company_type, $movie_companies, $role_type, $title) { return ($rt['id'] == $ci['role_id']); } ],
		[ 'items' => $title, 'on' => function($chn, $ci, $rt, $t) use ($cast_info, $char_name, $company_name, $company_type, $movie_companies, $role_type, $title) { return ($t['id'] == $ci['movie_id']); } ],
		[ 'items' => $movie_companies, 'on' => function($chn, $ci, $rt, $t, $mc) use ($cast_info, $char_name, $company_name, $company_type, $movie_companies, $role_type, $title) { return ($mc['movie_id'] == $t['id']); } ],
		[ 'items' => $company_name, 'on' => function($chn, $ci, $rt, $t, $mc, $cn) use ($cast_info, $char_name, $company_name, $company_type, $movie_companies, $role_type, $title) { return ($cn['id'] == $mc['company_id']); } ],
		[ 'items' => $company_type, 'on' => function($chn, $ci, $rt, $t, $mc, $cn, $ct) use ($cast_info, $char_name, $company_name, $company_type, $movie_companies, $role_type, $title) { return ($ct['id'] == $mc['company_type_id']); } ]
	], [ 'select' => function($chn, $ci, $rt, $t, $mc, $cn, $ct) use ($cast_info, $char_name, $company_name, $company_type, $movie_companies, $role_type, $title) { return ["character" => $chn['name'], "movie" => $t['title']]; }, 'where' => function($chn, $ci, $rt, $t, $mc, $cn, $ct) use ($cast_info, $char_name, $company_name, $company_type, $movie_companies, $role_type, $title) { return ((((((is_array($ci['note']) ? (array_key_exists("(voice)", $ci['note']) || in_array("(voice)", $ci['note'], true)) : (is_string($ci['note']) ? strpos($ci['note'], strval("(voice)")) !== false : false)) && (is_array($ci['note']) ? (array_key_exists("(uncredited)", $ci['note']) || in_array("(uncredited)", $ci['note'], true)) : (is_string($ci['note']) ? strpos($ci['note'], strval("(uncredited)")) !== false : false))) && ($cn['country_code'] == "[ru]")) && ($rt['role'] == "actor")) && ($t['production_year'] > 2005))); } ]);
})();
$result = [["uncredited_voiced_character" => min((function() use ($matches) {
	$res = [];
	foreach ((is_string($matches) ? str_split($matches) : $matches) as $x) {
		$res[] = $x['character'];
	}
	return $res;
})()), "russian_movie" => min((function() use ($matches) {
	$res = [];
	foreach ((is_string($matches) ? str_split($matches) : $matches) as $x) {
		$res[] = $x['movie'];
	}
	return $res;
})())]];
echo json_encode($result), PHP_EOL;
mochi_test_Q10_finds_uncredited_voice_actor_in_Russian_movie();

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
