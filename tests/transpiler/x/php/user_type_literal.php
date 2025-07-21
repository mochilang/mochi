<?php
$book = ["title" => "Go", "author" => ["name" => "Bob", "age" => 42]];
echo rtrim((is_float($book["author"]["name"]) ? sprintf("%.15f", $book["author"]["name"]) : $book["author"]["name"])), PHP_EOL;
?>
