#!/usr/bin/php

<?php

$source_machine = $argv[1]; // pn1cj9wcha0eko
$destination_machine = $argv[2]; // atjospd9jhdvoj
$destination_interface = $argv[3]; // 83

date_default_timezone_set("US/Pacific");
$west_01_username = "<username>";
$west_01_api_key = "50e937350b16d5829b9f9dd46904a2c3ef5d06c1";
$west_01_url = sprintf("http://west01.cloud.reliam.com/virtual_machines/%s/firewall_rules.json",$source_machine);

$west_02_username = "jb@popularllc.com";
$west_02_api_key = "15add47ac480e55c0107742eef75c9c77256c2f8";
$west_02_url = sprintf("http://west01.cloud.reliam.com/virtual_machines/%s/firewall_rules.json",$destination_machine);

$firewall_rules = onapp_api($west_01_username, $west_01_api_key, $west_01_url);

foreach($firewall_rules as $firewall_rule){
	
	$address = $firewall_rule['firewall_rule']['address'];

	
}






function onapp_api($username,$api_key,$url){

	$ch = curl_init();

	curl_setopt($ch, CURLOPT_URL, $url);
	curl_setopt($ch, CURLOPT_USERPWD, "$username:$api_key");
	curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_ANY);
	curl_setopt($ch, CURLOPT_HTTPGET, 1);
	curl_setopt($ch, CURLOPT_VERBOSE, 0);
	curl_setopt($ch, CURLOPT_HEADER, 0);
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

	$response = json_decode(curl_exec($ch), TRUE);

	curl_close($ch);
	
	return $response;
}



exit;


$server_template_variables = array(
	'hostname',
	'memory',
	'cpus',
	'booted',
	'hypervisor_id',
	'identifier',
	'suspended',
	'xen_id',
	'monthly_bandwidth_used',
	'total_disk_size',
	'updated_at',
	'created_at'
);

//hostname: pm-prod-app05
//memory: 512
//cpus: 1
//booted: 1
//hypervisor_id: 2
//identifier: lqakt061gj082c
//suspended:
//xen_id: 319
//monthly_bandwidth_used: 7630365.0
//total_disk_size: 20
//updated_at: 2014-02-05T09:12:49-08:00
//created_at: 2014-01-30T11:43:52-08:00


foreach ($servers as $virtual_server) {

	$server = $virtual_server['virtual_machine'];
	$updated_data = array();

	$server_id = get_server_id_by_identifier($mysqli, $server['identifier']);
	//    "ip_addresses": [
	//                {
	//                    "ip_address": {
	//                    "address": "<ip_address>",

	//print_r($server['ip_addresses'][0]['ip_address']['address']);

	$ip_address = $server['ip_addresses'][0]['ip_address']['address'];

	//printf("%s: %s %s\n", $server['identifier'], $server_id, $ip_address);
	update_server($mysqli, $server_id, 'ip_address', $ip_address);

	foreach ($server_template_variables as $server_template_variable) {
		update_server($mysqli, $server_id, $server_template_variable, $server[$server_template_variable]);
	}
}

printf("\n\n");

function update_server($mysqli, $server_id, $key, $value) {

	$sql = "UPDATE servers set {$key}='{$value}' WHERE server_id='{$server_id}'";
	$mysqli->query($sql);
}

function get_server_id_by_identifier($mysqli, $identifier) {

	$server_id = get_server_id($mysqli, $identifier);

	if (empty($server_id)) {
		$mysqli->query("INSERT INTO servers (identifier) VALUES ('{$identifier}')");
		$server_id = get_server_id($mysqli, $identifier);
	}

	return $server_id;

}

function get_server_id($mysqli, $identifier) {

	if ($mysqli->connect_error) {
		die('Connect Error (' . $mysqli->connect_errno . ') ' . $mysqli->connect_error);
	}

	$sql = "SELECT *,concat(hostname,'.',domain) as server_name FROM servers WHERE identifier='{$identifier}'";
	$server_id = "";

	if ($result = $mysqli->query($sql, MYSQLI_USE_RESULT)) {

		while ($row = $result->fetch_array()) {
			$server_id = $row['server_id'];
			printf("Updated %s\n", $row['server_name']);
		}
		$result->close();
	}

	return $server_id;

}

function clear_servers($mysqli) {

	if ($mysqli->connect_error) {
		die('Connect Error (' . $mysqli->connect_errno . ') ' . $mysqli->connect_error);
	}

	$sql = "UPDATE servers set status_id=0 WHERE instance_type_id=1";
	$server_id = "";

	if ($result = $mysqli->query($sql, MYSQLI_USE_RESULT)) {

		while ($row = $result->fetch_array()) {
			$server_id = $row['server_id'];
			printf("Updated %s\n", $row['server_name']);
		}
		$result->close();
	}

	return $server_id;

}
























