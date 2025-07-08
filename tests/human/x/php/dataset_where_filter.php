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
var_dump("--- Adults ---");
foreach ($adults as $person) {
    $extra = $person['is_senior'] ? " (senior)" : "";
    var_dump($person['name'], "is", $person['age'] . $extra);
}
?>
