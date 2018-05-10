<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 4/18/18
 * Time: 17:09
 */

$username = "David@popularllc.com";
$password = "P0pular1";
$list_id = "849";

$topica_command = new stdClass();
$topica_command->topicaAction = new stdClass();
$topica_command->topicaAction->serverPing = new stdClass();

//$topica_command->topicaAction->username = $username;
//$topica_command->topicaAction->password = $password;
//$topica_command->topicaAction->subscriberCount = array("list" => $list_id);


file_put_contents("/tmp/topica-import-{$list_id}-request.json", json_encode($topica_command));

$ch = curl_init("http://app.topicaplus.com/api/Dispatcher/?serverPing");
curl_setopt($ch, CURLOPT_POST, 1);
curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($topica_command));
curl_setopt($ch, CURLOPT_FOLLOWLOCATION, 1);
curl_setopt($ch, CURLOPT_HEADER, 0);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_HTTPHEADER, array("Content-Type: text/json"));

$response = curl_exec($ch);

file_put_contents("/tmp/topica-import-{$list_id}-response.json", $response);

$topica_response = json_decode($response);

if (json_last_error()) {
	$json_error = json_last_error();
} else {
	if ($topica_response->status != "OK") {
		$topica_error = json_encode($topica_response);
	}
}

if (curl_errno($ch)) {
	$curl_error = curl_error($ch);
}

if ($json_error || $topica_error || $curl_error) {
	print_r(array(
		'list_id' => $list_id,
		'total_records' => count($owner_posts),
		'curl_error' => curl_error($ch),
		'topica_response' => $topica_response,
		'response' => $response,
		'topica_command' => $topica_command,
	));
}

print_r($topica_response);
