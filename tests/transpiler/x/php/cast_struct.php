<?php
$todo = ["title" => "hi"];
echo rtrim((is_float($todo["title"]) ? sprintf("%.15f", $todo["title"]) : $todo["title"])), PHP_EOL;
?>
