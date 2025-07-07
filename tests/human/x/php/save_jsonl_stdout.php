<?php
$people = [
    ["name"=>"Alice", "age"=>30],
    ["name"=>"Bob", "age"=>25],
];
foreach ($people as $p) {
    echo json_encode($p) . PHP_EOL;
}
?>
