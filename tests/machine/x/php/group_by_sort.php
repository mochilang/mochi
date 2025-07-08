<?php
// items: [{string: any}]
$items = [["cat" => "a", "val" => 3], ["cat" => "a", "val" => 1], ["cat" => "b", "val" => 5], ["cat" => "b", "val" => 2]];
// grouped: [{string: any}]
$grouped = (function() use ($items) {
	$_src = (is_string($items) ? str_split($items) : $items);
	$_groups = _group_by($_src, function($i) use ($items) { return $i['cat']; });
	$pairs = [];
	foreach ($_groups as $g) {
		$pairs[] = ['item' => $g, 'key' => -array_sum((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['val'];
	}
	return $res;
})())];
	}
	usort($pairs, function($a, $b) {
		$ak = $a['key']; $bk = $b['key'];
		if (is_int($ak) && is_int($bk)) return $ak <=> $bk;
		if (is_string($ak) && is_string($bk)) return $ak <=> $bk;
		return strcmp(strval($ak), strval($bk));
	});
	$_groups = array_map(fn($p) => $p['item'], $pairs);
	$res = [];
	foreach ($_groups as $g) {
		$res[] = ["cat" => $g->key, "total" => array_sum((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['val'];
	}
	return $res;
})())];
	}
	return $res;
})();
_print($grouped);

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
