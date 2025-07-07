<?php
// people: [{string: string}]
$people = [["name" => "Alice", "city" => "Paris"], ["name" => "Bob", "city" => "Hanoi"], ["name" => "Charlie", "city" => "Paris"], ["name" => "Diana", "city" => "Hanoi"], ["name" => "Eve", "city" => "Paris"], ["name" => "Frank", "city" => "Hanoi"], ["name" => "George", "city" => "Paris"]];
// big: [{string: any}]
$big = (function() use ($people) {
	$_src = (is_string($people) ? str_split($people) : $people);
	$_groups = _group_by($_src, function($p) use ($people) { return $p['city']; });
	$res = [];
	foreach ($_groups as $g) {
		$res[] = ["city" => $g->key, "num" => (is_array($g->Items) ? count($g->Items) : strlen($g->Items))];
	}
	return $res;
})();
echo json_encode($big), PHP_EOL;

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
