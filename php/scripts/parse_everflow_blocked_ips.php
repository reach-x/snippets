<?php

$filename = "../../tmp/networks.json";

if (!file_exists($filename)) {
	die("File not found");
}

$file = file_get_contents($filename);

$dataset = json_decode($file, TRUE);

$ip_ranges = $dataset['relationship']['ips']['entries'];

foreach ($ip_ranges as $ip_range) {
	printf("whois %s\n", $ip_range['ip_from']);
	printf("sleep 5\n");
}
