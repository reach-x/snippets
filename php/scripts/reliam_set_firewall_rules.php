#!/usr/bin/php

<?php

if ( !isset($argv[1]) ) {
    printf("%s <virtual machine id> <network interface id>\n", $argv[0]);
    exit;
}

$username = "<username>";
$api_key = "50e937350b16d5829b9f9dd46904a2c3ef5d06c1";
$virtual_machine_id = $argv[1];
$network_interface_id = $argv[2];
$base_url = "http://west01.cloud.reliam.com";

//west01.cloud.reliam.com/virtual_machines/a86uegvw4q3g83/network_interfaces.json
//[
//    {
//        network_interface: {
//        created_at: "2014-02-25T12:45:43-08:00",
//        default_firewall_rule: "ACCEPT",
//        id: 359,
//        identifier: "noyau5i2ej6jgo",
//        label: "eth0",
//        mac_address: "00:16:3e:24:23:64",
//        network_join_id: 34,
//        primary: true,
//        rate_limit: 1,
//        updated_at: "2014-02-25T12:45:43-08:00",
//        usage: null,
//        usage_last_reset_at: null,
//        usage_month_rolled_at: null,
//        virtual_machine_id: 346
//    }
//    }


print_r(run_onapp_command("$base_url/virtual_machines/{$virtual_machine_id}/network_interfaces/{$network_interface_id}.json", "PUT", $username, $api_key, array( "network_interface" => array( "default_firewall_rule" => "DROP" ) )));
exit;

$firewall_rules = array(
    "firewall_rule" => array(
        "address" => "0.0.0.0/0",
        "command" => "ACCEPT",
        "protocol" => "TCP",
        "network_interface_id" => $network_interface_id,
        "port" => "80"
    ),
    "firewall_rule" => array(
        "address" => "0.0.0.0/0",
        "command" => "ACCEPT",
        "protocol" => "TCP",
        "network_interface_id" => $network_interface_id,
        "port" => "443"
    ),
    "firewall_rule" => array(
        "address" => "<ip_address>/32",
        "command" => "ACCEPT",
        "protocol" => "TCP",
        "network_interface_id" => $network_interface_id,
        "port" => "22"
    ),
    "firewall_rule" => array(
        "address" => "<ip_address>/32",
        "command" => "ACCEPT",
        "protocol" => "TCP",
        "network_interface_id" => $network_interface_id,
        "port" => "22"
    ),
    "firewall_rule" => array(
        "address" => "23.240.164.96/32",
        "command" => "ACCEPT",
        "protocol" => "TCP",
        "network_interface_id" => $network_interface_id,
        "port" => "22"
    ),
    "firewall_rule" => array(
        "address" => "173.198.16.11/32",
        "command" => "ACCEPT",
        "protocol" => "TCP",
        "network_interface_id" => $network_interface_id,
        "port" => "22"
    ),
    "firewall_rule" => array(
        "address" => "70.112.90.179/32",
        "command" => "ACCEPT",
        "protocol" => "TCP",
        "network_interface_id" => $network_interface_id,
        "port" => "22"
    ),
    "firewall_rule" => array(
        "address" => "70.114.188.147/32",
        "command" => "ACCEPT",
        "protocol" => "TCP",
        "network_interface_id" => $network_interface_id,
        "port" => "22"
    ),
    "firewall_rule" => array(
        "address" => "76.14.48.8/32",
        "command" => "ACCEPT",
        "protocol" => "TCP",
        "network_interface_id" => $network_interface_id,
        "port" => "22"
    ),
    "firewall_rule" => array(
        "address" => "166.137.182.20/32",
        "command" => "ACCEPT",
        "protocol" => "TCP",
        "network_interface_id" => $network_interface_id,
        "port" => "22"
    ),
);


foreach ( $firewall_rules as $firewall_rule ) {
    print_r(run_onapp_command("$base_url/virtual_machines/{$virtual_machine_id}/firewall_rules.json", "PUT", $username, $api_key, $firewall_rule));
}


/**
 * @param $url
 * @param $username
 * @param $api_key
 * @param $payload
 */
function run_onapp_command( $url, $method, $username, $api_key, $payload ) {

    $ch = curl_init();

    curl_setopt($ch, CURLOPT_URL, $url);
    curl_setopt($ch, CURLOPT_USERPWD, "$username:$api_key");
    curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_ANY);
    curl_setopt($ch, CURLOPT_HTTPGET, count($payload) == 0 ? 1 : 0);
    curl_setopt($ch, CURLOPT_POST, 1);
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $method);
    curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload));
    curl_setopt($ch, CURLOPT_VERBOSE, 1);
    curl_setopt($ch, CURLOPT_HEADER, 1);
    curl_setopt($ch, CURLOPT_HTTPHEADER, array('Content-type: application/json'));
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

    printf("%s\n", curl_exec($ch));

    $response = json_decode(curl_exec($ch), JSON_OBJECT_AS_ARRAY);

    curl_close($ch);

    return $response;
}















