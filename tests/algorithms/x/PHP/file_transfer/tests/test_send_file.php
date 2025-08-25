<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function make_conn_mock() {
  return ['close_called' => 0, 'recv_called' => 0, 'send_called' => 0];
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
function make_socket_mock(&$conn) {
  return ['accept_called' => 0, 'bind_called' => 0, 'close_called' => 0, 'conn' => &$conn, 'listen_called' => 0, 'shutdown_called' => 0];
}
function mochi_socket_bind(&$sock) {
  $sock['bind_called'] = $sock['bind_called'] + 1;
}
function mochi_socket_listen(&$sock) {
  $sock['listen_called'] = $sock['listen_called'] + 1;
}
function &mochi_socket_accept(&$sock) {
  $sock['accept_called'] = $sock['accept_called'] + 1;
  return $sock['conn'];
}
function mochi_socket_shutdown(&$sock) {
  $sock['shutdown_called'] = $sock['shutdown_called'] + 1;
}
function mochi_socket_close(&$sock) {
  $sock['close_called'] = $sock['close_called'] + 1;
}
function make_file_mock($values) {
  return ['data' => &$values, 'read_called' => 0];
}
function file_read(&$f, $size) {
  if ($f['read_called'] < count($f['data'])) {
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
  mochi_socket_bind($sock);
  mochi_socket_listen($sock);
  $conn =& mochi_socket_accept($sock);
  $_ = conn_recv($conn, 1024);
  $data = file_read($f, 1024);
  while ($data != 0) {
  conn_send($conn, $data);
  $data = file_read($f, 1024);
};
  conn_close($conn);
  mochi_socket_shutdown($sock);
  mochi_socket_close($sock);
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
