<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"]];
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 3, "total" => 80]];
$result = _query($orders, [['items'=>$customers, 'on'=>function($o, $c){return $o->customerId == $c->id;}, 'left'=>true]], [ 'select' => function($o, $c){return ["orderId" => $o->id, "customer" => $c, "total" => $o->total];} ]);
var_dump("--- Left Join ---");
foreach ($result as $entry) {
    var_dump("Order", $entry->orderId, "customer", $entry->customer, "total", $entry->total);
}
function _query($src, $joins, $opts) {
    $items = [];
    foreach ($src as $v) { $items[] = [$v]; }
    foreach ($joins as $j) {
        $joined = [];
        $jitems = $j['items'] ?? [];
        if (($j['right'] ?? false) && ($j['left'] ?? false)) {
            $matched = array_fill(0, count($jitems), false);
            foreach ($items as $left) {
                $m = false;
                foreach ($jitems as $ri => $right) {
                    $keep = true;
                    if (isset($j['on'])) {
                        $args = $left; $args[] = $right;
                        $keep = $j['on'](...$args);
                    }
                    if (!$keep) continue;
                    $m = true; $matched[$ri] = true;
                    $row = $left; $row[] = $right;
                    $joined[] = $row;
                }
                if (!$m) { $row = $left; $row[] = null; $joined[] = $row; }
            }
            foreach ($jitems as $ri => $right) {
                if (!$matched[$ri]) {
                    $undef = count($items) > 0 ? array_fill(0, count($items[0]), null) : [];
                    $row = $undef; $row[] = $right; $joined[] = $row;
                }
            }
        } elseif (($j['right'] ?? false)) {
            foreach ($jitems as $right) {
                $m = false;
                foreach ($items as $left) {
                    $keep = true;
                    if (isset($j['on'])) {
                        $args = $left; $args[] = $right;
                        $keep = $j['on'](...$args);
                    }
                    if (!$keep) continue;
                    $m = true; $row = $left; $row[] = $right; $joined[] = $row;
                }
                if (!$m) { $undef = count($items) > 0 ? array_fill(0, count($items[0]), null) : []; $row = $undef; $row[] = $right; $joined[] = $row; }
            }
        } else {
            foreach ($items as $left) {
                $m = false;
                foreach ($jitems as $right) {
                    $keep = true;
                    if (isset($j['on'])) {
                        $args = $left; $args[] = $right;
                        $keep = $j['on'](...$args);
                    }
                    if (!$keep) continue;
                    $m = true; $row = $left; $row[] = $right; $joined[] = $row;
                }
                if (($j['left'] ?? false) && !$m) { $row = $left; $row[] = null; $joined[] = $row; }
            }
        }
        $items = $joined;
    }
    if (isset($opts['where'])) {
        $fn = $opts['where'];
        $items = array_values(array_filter($items, fn($r) => $fn(...$r)));
    }
    if (isset($opts['sortKey'])) {
        $sk = $opts['sortKey'];
        usort($items, function($a,$b) use($sk) {
            $ak = $sk(...$a); $bk = $sk(...$b);
            if (is_array($ak) || is_object($ak)) $ak = json_encode($ak);
            if (is_array($bk) || is_object($bk)) $bk = json_encode($bk);
            return $ak <=> $bk;
        });
    }
    if (isset($opts['skip'])) {
        $n = $opts['skip']; if ($n < 0) $n = 0; $items = array_slice($items, $n);
    }
    if (isset($opts['take'])) {
        $n = $opts['take']; if ($n < 0) $n = 0; $items = array_slice($items, 0, $n);
    }
    $res = [];
    $sel = $opts['select'];
    foreach ($items as $r) { $res[] = $sel(...$r); }
    return $res;
}