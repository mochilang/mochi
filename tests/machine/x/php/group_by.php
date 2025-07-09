<?php
$people = [[$name => "Alice", $age => 30, $city => "Paris"], [$name => "Bob", $age => 15, $city => "Hanoi"], [$name => "Charlie", $age => 65, $city => "Paris"], [$name => "Diana", $age => 45, $city => "Hanoi"], [$name => "Eve", $age => 70, $city => "Paris"], [$name => "Frank", $age => 22, $city => "Hanoi"]];
$stats = (function() {
    $groups = [];
    foreach ($people as $person) {
        $_k = json_encode($person->city);
        $groups[$_k][] = $person;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $g = ['key'=>json_decode($_k, true),'items'=> $__g];
        $result[] = [$city => $g->key, $count => count($g['items']), $avg_age => (count((function() {
    $result = [];
    foreach ($g as $p) {
        $result[] = $p->age;
    }
    return $result;
})()) ? array_sum((function() {
    $result = [];
    foreach ($g as $p) {
        $result[] = $p->age;
    }
    return $result;
})())/count((function() {
    $result = [];
    foreach ($g as $p) {
        $result[] = $p->age;
    }
    return $result;
})()) : 0)];
    }
    return $result;
})();
var_dump("--- People grouped by city ---");
foreach ($stats as $s) {
    var_dump($s->city, ": count =", $s->count, ", avg_age =", $s->avg_age);
}
