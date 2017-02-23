<?php

$ip_addresses = file("tmp/ips.txt");
$ips = array();

foreach ($ip_addresses as $ip_address) {
	$ips[] = trim($ip_address);
}

file_put_contents("tmp/ips.json", json_encode($ips));
