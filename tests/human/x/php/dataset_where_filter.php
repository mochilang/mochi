<?php
$people = [
    ["name"=>"Alice","age"=>30],
    ["name"=>"Bob","age"=>15],
    ["name"=>"Charlie","age"=>65],
    ["name"=>"Diana","age"=>45],
];
$adults = [];
foreach ($people as $person) {
    if ($person['age'] >= 18) {
        $adults[] = [
            "name" => $person['name'],
            "age" => $person['age'],
            "is_senior" => $person['age'] >= 60,
        ];
    }
}
_print("--- Adults ---");
foreach ($adults as $person) {
    $extra = $person['is_senior'] ? " (senior)" : "";
    _print($person['name'], "is", $person['age'] . $extra);
}

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_array($a) || is_object($a)) {
            $parts[] = json_encode($a);
        } else {
            $parts[] = strval($a);
        }
    }
    echo implode(' ', $parts), PHP_EOL;
}
?>
