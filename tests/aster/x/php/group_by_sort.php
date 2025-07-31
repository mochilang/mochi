<?php
$items = [["cat" => "a", "val" => 3], ["cat" => "a", "val" => 1], ["cat" => "b", "val" => 5], ["cat" => "b", "val" => 2]];
$grouped = (function() use ($items) {
    $groups = [];
    foreach ($items as $i) {
        $key = $i["cat"];
        $k = json_encode($key);
        if (!array_key_exists($k, $groups)) {
            $groups[$k] = ["key" => $key, "items" => []];
        }
        $groups[$k]["items"][] = $i;
    }
    $result = [];
    foreach ($groups as $g) {
        $result[] = [-array_sum((function() use ($i, $g) {
    $result = [];
    foreach ($g["items"] as $x) {
        $result[] = $x["val"];
    }
    return $result;
}
)()), ["cat" => $g["key"], "total" => array_sum((function() use ($i, $g) {
    $result = [];
    foreach ($g["items"] as $x) {
        $result[] = $x["val"];
    }
    return $result;
}
)())]];
    }
    usort($result, function($a, $b) {
    return $a[0] <=> $b[0];
}
);
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
}
)();
echo str_replace("false", "False", str_replace("true", "True", str_replace("'", str_replace(":", ": ", str_replace(",", ", ", json_encode($grouped, 1344)))))), PHP_EOL;
