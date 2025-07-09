<?php
$nations = [["id" => 1, "name" => "A"], ["id" => 2, "name" => "B"]];
$suppliers = [["id" => 1, "nation" => 1], ["id" => 2, "nation" => 2]];
$partsupp = [["part" => 100, "supplier" => 1, "cost" => 10, "qty" => 2], ["part" => 100, "supplier" => 2, "cost" => 20, "qty" => 1], ["part" => 200, "supplier" => 1, "cost" => 5, "qty" => 3]];
$filtered = (function() {
    $result = [];
    foreach ($partsupp as $ps) {
        foreach ($suppliers as $s) {
            if ($s->id == $ps->supplier) {
                foreach ($nations as $n) {
                    if ($n->id == $s->nation) {
                        if ($n->name == "A") {
                            $result[] = ["part" => $ps->part, "value" => $ps->cost * $ps->qty];
                        }
                    }
                }
            }
        }
    }
    return $result;
})();
$grouped = (function() {
    $groups = [];
    foreach ($filtered as $x) {
        $_k = json_encode($x->part);
        $groups[$_k][] = $x;
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $g = ['key'=>json_decode($_k, true),'items'=> $__g];
        $result[] = ["part" => $g->key, "total" => array_sum((function() {
    $result = [];
    foreach ($g as $r) {
        $result[] = $r->value;
    }
    return $result;
})())];
    }
    return $result;
})();
var_dump($grouped);
