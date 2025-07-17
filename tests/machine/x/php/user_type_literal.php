<?php
class Person {
    public $name;
    public $age;
    public function __construct($fields = []) {
        $this->name = $fields['name'] ?? null;
        $this->age = $fields['age'] ?? null;
    }
}
class Book {
    public $title;
    public $author;
    public function __construct($fields = []) {
        $this->title = $fields['title'] ?? null;
        $this->author = $fields['author'] ?? null;
    }
}
$book = new Book([
    'title' => "Go",
    'author' => new Person(['name' => "Bob", 'age' => 42])
]);
_print($book->author->name);
function _print(...$args) {
    $first = true;
    foreach ($args as $a) {
        if (!$first) echo ' ';
        $first = false;
        if (is_array($a)) {
            if (array_is_list($a)) {
                if ($a && is_array($a[0])) {
                    $parts = [];
                    foreach ($a as $sub) {
                        if (is_array($sub)) {
                            $parts[] = '[' . implode(' ', $sub) . ']';
                        } else {
                            $parts[] = strval($sub);
                        }
                    }
                    echo implode(' ', $parts);
                } else {
                    echo '[' . implode(' ', array_map('strval', $a)) . ']';
                }
            } else {
                echo json_encode($a);
            }
        } else {
            echo strval($a);
        }
    }
    echo PHP_EOL;
}
?>
