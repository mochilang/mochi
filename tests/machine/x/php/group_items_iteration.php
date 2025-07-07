<?php
// data: [{string: any}]
$data = [["tag" => "a", "val" => 1], ["tag" => "a", "val" => 2], ["tag" => "b", "val" => 3]];
// groups: [any]
$groups = (function() use ($data) {
	$_src = (is_string($data) ? str_split($data) : $data);
	$_groups = _group_by($_src, function($d) use ($data) { return $d['tag']; });
	$res = [];
	foreach ($_groups as $g) {
		$res[] = $g->Items;
	}
	return $res;
})();
// tmp: [any]
$tmp = [];
foreach ((is_string($groups) ? str_split($groups) : $groups) as $g) {
	// total: int
	$total = 0;
	foreach ((is_string($g['items']) ? str_split($g['items']) : $g['items']) as $x) {
		$total = ((is_array($total) && is_array($x['val'])) ? array_merge($total, $x['val']) : ((is_string($total) || is_string($x['val'])) ? ($total . $x['val']) : ($total + $x['val'])));
	}
	$tmp = array_merge($tmp, [["tag" => $g['key'], "total" => $total]]);
}
// result: [any]
$result = (function() use ($tmp) {
	$_src = $tmp;
	return _query($_src, [
	], [ 'select' => function($r) use ($tmp) { return $r; }, 'sortKey' => function($r) use ($tmp) { return ($r['tag']); } ]);
})();
_print($result);

class _Group {
    public $key;
    public $Items;
    function __construct($key) { $this->key = $key; $this->Items = []; }
}
function _group_by($src, $keyfn) {
    $groups = [];
    $order = [];
    foreach ($src as $it) {
        $key = $keyfn($it);
        if (is_array($key)) { $key = (object)$key; }
        $ks = is_object($key) ? json_encode($key) : strval($key);
        if (!isset($groups[$ks])) {
            $groups[$ks] = new _Group($key);
            $order[] = $ks;
        }
        $groups[$ks]->Items[] = $it;
    }
    $res = [];
    foreach ($order as $ks) { $res[] = $groups[$ks]; }
    return $res;
}
function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
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
