<?php
function _lookup_host($host) {
    $res = dns_get_record($host, DNS_A);
    if ($res === false) {
        return [[], "lookup failed"];
    }
    $ips = [];
    foreach ($res as $r) {
        if (isset($r['ip'])) { $ips[] = $r['ip']; }
    }
    return [$ips, null];
}
$net = ["LookupHost" => "_lookup_host"];
$res = $net['LookupHost']("www.kame.net");
$addrs = $res[0];
$err = $res[1];
if ($err == null) {
  echo json_encode(json_encode($addrs, 1344), 1344), PHP_EOL;
} else {
  echo json_encode($err, 1344), PHP_EOL;
}
