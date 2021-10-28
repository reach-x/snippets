<?php

if (isset($argv[1])) {
	$filename = $argv[1];
} else {
	die("Need argument of ips to script");
}

class database {

	public $username = "root";
	public $password = "<password>";
	public $name = "ip_info";
	public $server = "database.pm.internal";
	public $port = 3306;
}

$database = new database();
$lines = file($filename);
$first_row = TRUE;


foreach ($lines as $line) {

	$ip_address = trim($line);
	$ip_address_number = ip2long($ip_address);
	$sql = sprintf("SELECT * FROM ip_info.ip_address_data WHERE ip_to > '%s' LIMIT 1", $ip_address_number);
	$mysqli = new mysqli($database->server, $database->username, $database->password, $database->name, $database->port);

	if ($mysqli->connect_errno) {
		printf("Error:%s %s \n", $mysqli->connect_errno, $mysqli->connect_error);
	}

	$result = $mysqli->query($sql);
	$row = $result->fetch_array(MYSQLI_ASSOC);

	if ($first_row) {
		printf("ip_address,%s\n", join(",", array_keys($row)));
		$first_row = FALSE;
	}

	printf("%s,%s\n", $ip_address, join(",", $row));
}
