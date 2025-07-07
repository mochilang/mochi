<?php
// people: [{string: any}]
$people = [["name" => "Alice", "age" => 30], ["name" => "Bob", "age" => 25]];
_save_json($people, "-");

function _save_json($rows, $path) {
    $out = json_encode($rows);
    if ($path === '' || $path === '-') { fwrite(STDOUT, $out . PHP_EOL); } else { file_put_contents($path, $out); }
}
