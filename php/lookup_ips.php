<?php

class database {

	public $username = "root";
	public $password = "4RXisthefuture!";
	public $database = "ip_info";
	public $name = "ip_info";
	public $server = 'db01.mypm.us';
	public $port = 3306;
}

$database = new database();

$mysqli = new mysqli($database->server, $database->username, $database->password, 'lbc', 3306);


if ($mysqli->connect_error) {
	die('Connect Error (' . $mysqli->connect_errno . ') ' . $mysqli->connect_error);
} else {
	printf("Connected to main db\n");
}

$result = $mysqli->query("SELECT * FROM lbc.redirects where created_at BETWEEN '2016-02-29 00:00:00' AND '2016-02-29 23:59:59' and manager_post_id is not null and ip_address is not null");

$rows = array();

while ($row = $result->fetch_assoc()) {
	$rows[] = $row;
}

$ip_addresses = array();
$usage_types = array();

foreach ($rows as $row) {

	$sql = sprintf("select * from ip_info.ip_address_data where ip_to > INET_ATON('%s') limit 1", $row['ip_address']);
	$result = $mysqli->query($sql);
	$ip_address = $result->fetch_assoc();

	if (!isset($usage_types[$ip_address['usage_type']])) {
		$usage_types[$ip_address['usage_type']] = array(
			'hits' => 0,
			'amount' => -0,
		);
	}

	$usage_types[$ip_address['usage_type']]['hits']++;

	if (isset($row['manager_post_id']) && $row['manager_post_id'] > 0) {

		$sql = sprintf("select sum(amount) amount from lbc.campaign_revenue where manager_post_id=%d and `date` BETWEEN '2016-02-29 00:00:00' AND '2016-03-02 23:59:59'", $row['manager_post_id']);
		$result = $mysqli->query($sql);
		$revenue = $result->fetch_assoc();
		//print_r(array('$revenue' => $revenue, '$sql' => $sql));
		$usage_types[$ip_address['usage_type']]['amount'] = round(floatval($usage_types[$ip_address['usage_type']]['amount']) + floatval($revenue['amount']), 2);
	}

}

print_r($usage_types);

