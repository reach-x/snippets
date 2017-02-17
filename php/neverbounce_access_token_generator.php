<?php

define('DATA_SERVICE_NONE', 0);
define('DATA_SERVICE_IMPRESSIONWISE', 1);
define('DATA_SERVICE_XVERIFY', 2);
define('DATA_SERVICE_REACHX', 3);
define('DATA_SERVICE_NEVERBOUNCE', 4);

$credentials = array(
	DATA_SERVICE_NONE => array(
		'username' => "",
		'password' => "",
	),
	DATA_SERVICE_IMPRESSIONWISE => array(
		'username' => "779005",
		'password' => "ReaX1",
	),
	DATA_SERVICE_REACHX => array(
		'username' => "",
		'password' => "",
	),
	DATA_SERVICE_XVERIFY => array(
		'username' => "",
		'password' => "",
	),
	DATA_SERVICE_NEVERBOUNCE => array(
		'username' => "popularllc",
		'password' => "m8nAeTbU",
	),
);

$owner_post = array();
$owner_post['data_service_type_id'] = DATA_SERVICE_NEVERBOUNCE;

if ($owner_post['data_service_type_id'] == DATA_SERVICE_NEVERBOUNCE) {
	if (!isset($credentials[DATA_SERVICE_NEVERBOUNCE]['access_token'])) {
		get_neverbounce_token($credentials);
		printf("getting token %s\n", $credentials[DATA_SERVICE_NEVERBOUNCE]['access_token']);

	} else {
		printf("already have token %s\n", $credentials[DATA_SERVICE_NEVERBOUNCE]['access_token']);
	}
}

if ($owner_post['data_service_type_id'] == DATA_SERVICE_NEVERBOUNCE) {
	if (!isset($credentials[DATA_SERVICE_NEVERBOUNCE]['access_token'])) {
		get_neverbounce_token($credentials);
		printf("getting token %s\n", $credentials[DATA_SERVICE_NEVERBOUNCE]['access_token']);

	} else {
		printf("already have token %s\n", $credentials[DATA_SERVICE_NEVERBOUNCE]['access_token']);
	}
}

print_r(get_neverbounce_token($credentials));


function get_neverbounce_token(&$credentials) {

	$ch = curl_init("https://popularllc.neverbounce.com/v3/access_token");
	curl_setopt_array($ch, array(
			//		                  CURLOPT_VERBOSE => TRUE,
			//		                  CURLOPT_HEADER => TRUE,
			CURLOPT_POST => TRUE,
			CURLOPT_POSTFIELDS => array(
				"grant_type" => 'client_credentials',
				"scope" => 'basic user',
			),
			CURLOPT_USERPWD => sprintf("%s:%s", $credentials[DATA_SERVICE_NEVERBOUNCE]['username'], $credentials[DATA_SERVICE_NEVERBOUNCE]['password']),
			CURLOPT_RETURNTRANSFER => TRUE,
		));
	$raw_response = curl_exec($ch);
	$response = json_decode($raw_response, TRUE);
	$credentials[DATA_SERVICE_NEVERBOUNCE]['access_token'] = $response['access_token'];
}

