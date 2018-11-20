<?php

$string = file_get_contents($argv[1]);
$regex = "/(?<sid>.*),(?<report_date>.*),(?<c1>.*),(?<c2>.*),(?<c3>.*),(?<ip_address>.*),(?<amount>.*),(?<hit_id>.*),(?<earned>.*),(?<affiliate_id>.*)/mixX";
$regex = "/(.*),(.*),(.*),(.*),(.*),(.*),(.*),(.*),(.*),(.*)/mixX";
$regex = "/(.*)/m";
$response = preg_split($regex,$string);

print_r($response);

