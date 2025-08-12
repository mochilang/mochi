<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function get_imdb_top_250_movies($url) {
  $movies = ['The Shawshank Redemption' => 9.2, 'The Godfather' => 9.2, 'The Dark Knight' => 9.0];
  return $movies;
}
function write_movies($filename) {
  $movies = get_imdb_top_250_movies('');
  echo rtrim('Movie title,IMDb rating'), PHP_EOL;
  foreach (array_keys($movies) as $title) {
  $rating = $movies[$title];
  echo rtrim($title . ',' . _str($rating)), PHP_EOL;
};
}
write_movies('IMDb_Top_250_Movies.csv');
