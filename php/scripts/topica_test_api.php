<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 4/18/18
 * Time: 17:09
 */

$accounts = array(

	'rockwatermarketing' => array(
		'username' => "rockwatermarketing",
		'password' => "PTY0xZHZrm0YYmj0qTOF",
		'list_id' => "1231",
	),

	'popularmarketing' => array(
		'username' => "popularmarketing",
		'password' => "EOlmuA4XEkl08alXcTBC",
	),
	'gobankcards' => array(
		'username' => "gobankcards",
		'password' => "2fgqrDHPsPG3YAxRXQUY",
	),
	'everydayfinancehelp' => array(
		'username' => "everydayfinancehelp",
		'password' => "OqMsSBLwz1w5kjeGQDar",
	),
);

foreach ($accounts as $account) {
	$topica_command = new stdClass();
	$topica_command->topicaAction = new stdClass();
	$topica_command->topicaAction->account = $account['username'];
	$topica_command->topicaAction->password = $account['password'];
	$topica_command->topicaAction->serverPing = new stdClass();
	//$topica_command->topicaAction->listDetail = new stdClass();
	//$topica_command->topicaAction->subscriberCount = array("list" => $list_id);
	//file_put_contents("/tmp/topica-import-{$list_id}-request.json", json_encode($topica_command));

	printf("SENT: %s\n\n", json_encode($topica_command));

	$ch = curl_init("http://app.topicaplus.com/api/Dispatcher/");
	curl_setopt($ch, CURLOPT_POST, 1);
	curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($topica_command));
	curl_setopt($ch, CURLOPT_FOLLOWLOCATION, 1);
	curl_setopt($ch, CURLOPT_HEADER, 0);
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
	//curl_setopt($ch, CURLOPT_HTTPHEADER, array("Content-Type: application/json"));
	curl_setopt($ch, CURLOPT_HTTPHEADER, array("Content-Type: application/json"));

	$response = curl_exec($ch);

	//file_put_contents("/tmp/topica-import-{$list_id}-response.json", $response);

	$topica_response = json_decode($response);
	$json_error = FALSE;
	$topica_error = FALSE;
	$curl_error = FALSE;

	if (json_last_error()) {
		$json_error = json_last_error();
	} else {
		if ($topica_response->topicaReply->status != "OK") {
			$topica_error = json_encode($topica_response);
		}
	}

	if (curl_errno($ch)) {
		$curl_error = curl_error($ch);
	}

	print_r(array(
//		'account' => $account,
//		'curl_error' => curl_error($ch),
		'topica_response' => $topica_response->topicaReply,
//		'topica_command' => $topica_command,
	));
}
