<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function make_conn_mock() {
  return ['recv_called' => 0, 'send_called' => 0, 'close_called' => 0];
}
function conn_recv(&$conn, $size) {
  $conn['recv_called'] = $conn['recv_called'] + 1;
  return 0;
}
function conn_send(&$conn, $data) {
  $conn['send_called'] = $conn['send_called'] + 1;
}
function conn_close(&$conn) {
  $conn['close_called'] = $conn['close_called'] + 1;
}
function make_socket_mock($conn) {
  return ['bind_called' => 0, 'listen_called' => 0, 'accept_called' => 0, 'shutdown_called' => 0, 'close_called' => 0, 'conn' => $conn];
}
function socket_bind(&$sock) {
  $sock['bind_called'] = $sock['bind_called'] + 1;
}
function socket_listen(&$sock) {
  $sock['listen_called'] = $sock['listen_called'] + 1;
}
function socket_accept(&$sock) {
  $sock['accept_called'] = $sock['accept_called'] + 1;
  return $sock['conn'];
}
function socket_shutdown(&$sock) {
  $sock['shutdown_called'] = $sock['shutdown_called'] + 1;
}
function socket_close(&$sock) {
  $sock['close_called'] = $sock['close_called'] + 1;
}
function make_file_mock($values) {
  return ['read_called' => 0, 'data' => $values];
}
function file_read(&$f, $size) {
  if ($f['read_called'] < _len($f['data'])) {
  $value = $f['data'][$f['read_called']];
  $f['read_called'] = $f['read_called'] + 1;
  return $value;
}
  $f['read_called'] = $f['read_called'] + 1;
  return 0;
}
function file_open() {
  return make_file_mock([1, 0]);
}
function send_file(&$sock, &$f) {
  socket_bind($sock);
  socket_listen($sock);
  $conn = socket_accept($sock);
  $_ = conn_recv($conn, 1024);
  $data = file_read($f, 1024);
  while ($data != 0) {
  conn_send($conn, $data);
  $data = file_read($f, 1024);
};
  conn_close($conn);
  socket_shutdown($sock);
  socket_close($sock);
}
function test_send_file_running_as_expected() {
  $conn = make_conn_mock();
  $sock = make_socket_mock($conn);
  $f = file_open();
  send_file($sock, $f);
  if ($sock['bind_called'] == 1 && $sock['listen_called'] == 1 && $sock['accept_called'] == 1 && $conn['recv_called'] == 1 && $f['read_called'] >= 1 && $conn['send_called'] == 1 && $conn['close_called'] == 1 && $sock['shutdown_called'] == 1 && $sock['close_called'] == 1) {
  return 'pass';
}
  return 'fail';
}
echo rtrim(test_send_file_running_as_expected()), PHP_EOL;
