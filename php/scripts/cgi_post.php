<?php

$data = array(
	"LIST_KEY"      => "UDC_HHA",
	"FIRST_NAME"    => "John",
	"EMAIL_ADDRESS" => "john@gmail.com",
	"IP_ADDRESS"    => "1.2.3.4",
	"GENERATED_AT"  => "2023-05-11",
	"SOURCE_DOMAIN" => "popularmarketing.com",
	"PHONE"         => "5555551212",
);

$custom_user_agent = "HTTP Data Poster v1.0";

$endpoint_url = "https://secure.populardatamanagement.com/post/inbound";
$curl_handle = curl_init($endpoint_url);

curl_setopt_array($curl_handle, array(
	CURLOPT_POST           => TRUE,
	CURLOPT_POSTFIELDS     => $data,
	CURLOPT_RETURNTRANSFER => TRUE,
	CURLOPT_USERAGENT      => $custom_user_agent,
));

$curl_output = curl_exec($curl_handle);

$response = json_decode($curl_output);

if (json_last_error() > 0) {
	printf("There was an error processing the response\nERROR: %s\n", json_last_error_msg());
} else {
	print_r($response);
}