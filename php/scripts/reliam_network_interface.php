#!/usr/bin/php

<?php

if ( !isset($argv[1]) ) {
    printf("%s <virtual machine id> \n", $argv[0]);
    exit;
}

$virtual_machine_id = $argv[1];

$username = "<username>";
$api_key = "50e937350b16d5829b9f9dd46904a2c3ef5d06c1";

$base_url = "http://west01.cloud.reliam.com";
$network_interface = run_onapp_command("$base_url/virtual_machines/{$virtual_machine_id}/network_interfaces.json", "GET", $username, $api_key, array());
print_r($network_interface);
$network_interface_id = $network_interface[0]['network_interface']['id'];
printf("Found %s as network_interface_id\n", $network_interface_id);

function run_onapp_command( $url, $method, $username, $api_key, $payload ) {

    $ch = curl_init();

    curl_setopt($ch, CURLOPT_URL, $url);
    curl_setopt($ch, CURLOPT_USERPWD, "$username:$api_key");
    curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_ANY);
    curl_setopt($ch, CURLOPT_HTTPGET, count($payload) == 0 ? 1 : 0);
    curl_setopt($ch, CURLOPT_POST, 1);
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $method);
    curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload));
    curl_setopt($ch, CURLOPT_VERBOSE, 0);
    curl_setopt($ch, CURLOPT_HEADER, 0);
    curl_setopt($ch, CURLOPT_HTTPHEADER, array( 'Content-type: application/json' ));
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

    printf("%s\n", curl_exec($ch));

    $response = json_decode(curl_exec($ch), JSON_OBJECT_AS_ARRAY);

    curl_close($ch);

    return $response;
}



