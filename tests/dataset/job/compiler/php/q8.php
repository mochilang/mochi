<?php
function mochi_test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing() {
	global $actress_pseudonym, $japanese_movie_dubbed, $result;
	if (!(($result == [["actress_pseudonym" => "Y. S.", "japanese_movie_dubbed" => "Dubbed Film"]]))) { throw new Exception('expect failed'); }
}

$aka_name = [["person_id" => 1, "name" => "Y. S."]];
$cast_info = [["person_id" => 1, "movie_id" => 10, "note" => "(voice: English version)", "role_id" => 1000]];
$company_name = [["id" => 50, "country_code" => "[jp]"]];
$movie_companies = [["movie_id" => 10, "company_id" => 50, "note" => "Studio (Japan)"]];
$name = [["id" => 1, "name" => "Yoko Ono"], ["id" => 2, "name" => "Yuichi"]];
$role_type = [["id" => 1000, "role" => "actress"]];
$title = [["id" => 10, "title" => "Dubbed Film"]];
$eligible = (function() use ($aka_name, $cast_info, $company_name, $movie_companies, $name, $role_type, $title) {
	$_src = $aka_name;
	return _query($_src, [
		[ 'items' => $name, 'on' => function($an1, $n1) use ($aka_name, $cast_info, $company_name, $movie_companies, $name, $role_type, $title) { return ($n1['id'] == $an1['person_id']); } ],
		[ 'items' => $cast_info, 'on' => function($an1, $n1, $ci) use ($aka_name, $cast_info, $company_name, $movie_companies, $name, $role_type, $title) { return ($ci['person_id'] == $an1['person_id']); } ],
		[ 'items' => $title, 'on' => function($an1, $n1, $ci, $t) use ($aka_name, $cast_info, $company_name, $movie_companies, $name, $role_type, $title) { return ($t['id'] == $ci['movie_id']); } ],
		[ 'items' => $movie_companies, 'on' => function($an1, $n1, $ci, $t, $mc) use ($aka_name, $cast_info, $company_name, $movie_companies, $name, $role_type, $title) { return ($mc['movie_id'] == $ci['movie_id']); } ],
		[ 'items' => $company_name, 'on' => function($an1, $n1, $ci, $t, $mc, $cn) use ($aka_name, $cast_info, $company_name, $movie_companies, $name, $role_type, $title) { return ($cn['id'] == $mc['company_id']); } ],
		[ 'items' => $role_type, 'on' => function($an1, $n1, $ci, $t, $mc, $cn, $rt) use ($aka_name, $cast_info, $company_name, $movie_companies, $name, $role_type, $title) { return ($rt['id'] == $ci['role_id']); } ]
	], [ 'select' => function($an1, $n1, $ci, $t, $mc, $cn, $rt) use ($aka_name, $cast_info, $company_name, $movie_companies, $name, $role_type, $title) { return ["pseudonym" => $an1['name'], "movie_title" => $t['title']]; }, 'where' => function($an1, $n1, $ci, $t, $mc, $cn, $rt) use ($aka_name, $cast_info, $company_name, $movie_companies, $name, $role_type, $title) { return (((((((($ci['note'] == "(voice: English version)") && ($cn['country_code'] == "[jp]")) && (is_array($mc['note']) ? (array_key_exists("(Japan)", $mc['note']) || in_array("(Japan)", $mc['note'], true)) : (is_string($mc['note']) ? strpos($mc['note'], strval("(Japan)")) !== false : false))) && (!(is_array($mc['note']) ? (array_key_exists("(USA)", $mc['note']) || in_array("(USA)", $mc['note'], true)) : (is_string($mc['note']) ? strpos($mc['note'], strval("(USA)")) !== false : false)))) && (is_array($n1['name']) ? (array_key_exists("Yo", $n1['name']) || in_array("Yo", $n1['name'], true)) : (is_string($n1['name']) ? strpos($n1['name'], strval("Yo")) !== false : false))) && (!(is_array($n1['name']) ? (array_key_exists("Yu", $n1['name']) || in_array("Yu", $n1['name'], true)) : (is_string($n1['name']) ? strpos($n1['name'], strval("Yu")) !== false : false)))) && ($rt['role'] == "actress"))); } ]);
})();
$result = [["actress_pseudonym" => min((function() use ($eligible) {
	$res = [];
	foreach ((is_string($eligible) ? str_split($eligible) : $eligible) as $x) {
		$res[] = $x['pseudonym'];
	}
	return $res;
})()), "japanese_movie_dubbed" => min((function() use ($eligible) {
	$res = [];
	foreach ((is_string($eligible) ? str_split($eligible) : $eligible) as $x) {
		$res[] = $x['movie_title'];
	}
	return $res;
})())]];
_print($result);
mochi_test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing();

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
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
