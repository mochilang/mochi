<?php
function mochi_test_Q9_selects_minimal_alternative_name__character_and_movie() {
	global $alternative_name, $character_name, $movie, $result;
	if (!(($result == [["alternative_name" => "A. N. G.", "character_name" => "Angel", "movie" => "Famous Film"]]))) { throw new Exception('expect failed'); }
}

$aka_name = [["person_id" => 1, "name" => "A. N. G."], ["person_id" => 2, "name" => "J. D."]];
$char_name = [["id" => 10, "name" => "Angel"], ["id" => 20, "name" => "Devil"]];
$cast_info = [["person_id" => 1, "person_role_id" => 10, "movie_id" => 100, "role_id" => 1000, "note" => "(voice)"], ["person_id" => 2, "person_role_id" => 20, "movie_id" => 200, "role_id" => 1000, "note" => "(voice)"]];
$company_name = [["id" => 100, "country_code" => "[us]"], ["id" => 200, "country_code" => "[gb]"]];
$movie_companies = [["movie_id" => 100, "company_id" => 100, "note" => "ACME Studios (USA)"], ["movie_id" => 200, "company_id" => 200, "note" => "Maple Films"]];
$name = [["id" => 1, "name" => "Angela Smith", "gender" => "f"], ["id" => 2, "name" => "John Doe", "gender" => "m"]];
$role_type = [["id" => 1000, "role" => "actress"], ["id" => 2000, "role" => "actor"]];
$title = [["id" => 100, "title" => "Famous Film", "production_year" => 2010], ["id" => 200, "title" => "Old Movie", "production_year" => 1999]];
$matches = (function() use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) {
	$_src = $aka_name;
	return _query($_src, [
		[ 'items' => $name, 'on' => function($an, $n) use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) { return ($an['person_id'] == $n['id']); } ],
		[ 'items' => $cast_info, 'on' => function($an, $n, $ci) use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) { return ($ci['person_id'] == $n['id']); } ],
		[ 'items' => $char_name, 'on' => function($an, $n, $ci, $chn) use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) { return ($chn['id'] == $ci['person_role_id']); } ],
		[ 'items' => $title, 'on' => function($an, $n, $ci, $chn, $t) use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) { return ($t['id'] == $ci['movie_id']); } ],
		[ 'items' => $movie_companies, 'on' => function($an, $n, $ci, $chn, $t, $mc) use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) { return ($mc['movie_id'] == $t['id']); } ],
		[ 'items' => $company_name, 'on' => function($an, $n, $ci, $chn, $t, $mc, $cn) use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) { return ($cn['id'] == $mc['company_id']); } ],
		[ 'items' => $role_type, 'on' => function($an, $n, $ci, $chn, $t, $mc, $cn, $rt) use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) { return ($rt['id'] == $ci['role_id']); } ]
	], [ 'select' => function($an, $n, $ci, $chn, $t, $mc, $cn, $rt) use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) { return ["alt" => $an['name'], "character" => $chn['name'], "movie" => $t['title']]; }, 'where' => function($an, $n, $ci, $chn, $t, $mc, $cn, $rt) use ($aka_name, $cast_info, $char_name, $company_name, $movie_companies, $name, $role_type, $title) { return ((((((((((is_array(["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]) ? (array_key_exists($ci['note'], ["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]) || in_array($ci['note'], ["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"], true)) : (is_string(["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]) ? strpos(["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"], strval($ci['note'])) !== false : false))) && ($cn['country_code'] == "[us]")) && (((is_array($mc['note']) ? (array_key_exists("(USA)", $mc['note']) || in_array("(USA)", $mc['note'], true)) : (is_string($mc['note']) ? strpos($mc['note'], strval("(USA)")) !== false : false)) || (is_array($mc['note']) ? (array_key_exists("(worldwide)", $mc['note']) || in_array("(worldwide)", $mc['note'], true)) : (is_string($mc['note']) ? strpos($mc['note'], strval("(worldwide)")) !== false : false))))) && ($n['gender'] == "f")) && (is_array($n['name']) ? (array_key_exists("Ang", $n['name']) || in_array("Ang", $n['name'], true)) : (is_string($n['name']) ? strpos($n['name'], strval("Ang")) !== false : false))) && ($rt['role'] == "actress")) && ($t['production_year'] >= 2005)) && ($t['production_year'] <= 2015))); } ]);
})();
$result = [["alternative_name" => min((function() use ($matches) {
	$res = [];
	foreach ((is_string($matches) ? str_split($matches) : $matches) as $x) {
		$res[] = $x['alt'];
	}
	return $res;
})()), "character_name" => min((function() use ($matches) {
	$res = [];
	foreach ((is_string($matches) ? str_split($matches) : $matches) as $x) {
		$res[] = $x['character'];
	}
	return $res;
})()), "movie" => min((function() use ($matches) {
	$res = [];
	foreach ((is_string($matches) ? str_split($matches) : $matches) as $x) {
		$res[] = $x['movie'];
	}
	return $res;
})())]];
echo json_encode($result), PHP_EOL;
mochi_test_Q9_selects_minimal_alternative_name__character_and_movie();

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
