<?php

date_default_timezone_set("US/Pacific");
    
$url = "https://www.google.com";
$orignal_parse = parse_url($url, PHP_URL_HOST);
$get = stream_context_create(array("ssl" => array("capture_peer_cert" => TRUE)));
$read = stream_socket_client("ssl://".$orignal_parse.":443", $errno, $errstr, 30, STREAM_CLIENT_CONNECT, $get);
$cert = stream_context_get_params($read);
$certinfo = openssl_x509_parse($cert['options']['ssl']['peer_certificate']);

//printf("Expires: %s\n",date("Y-m-d H:i:s",$certinfo['validTo_time_t']));

$current_time = time();

printf("Now: %s\n",date("Y-m-d H:i:s",time()));
printf("Expires: %s\n",date("Y-m-d H:i:s",$certinfo['validTo_time_t']));
