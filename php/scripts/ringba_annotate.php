<?php

$inbound_call_id = "RGB890913B3F72EC40F36BBB8BDCC0032E9BCDF7F24V3D2K01";
$tag = new \stdClass();
$tag->tagType = "Annotations";
$tag->tagName = "Word";
$tag->tagValue = "Up";
$post_data = new \stdClass();
$post_data->inboundCallId = $inbound_call_id;
$post_data->tags = [$tag];

//printf("%s\n",json_encode($post_data,JSON_PRETTY_PRINT));
//exit;

$curl = curl_init();

curl_setopt_array($curl, array(
	CURLOPT_URL            => 'https://api.ringba.com/v2/RA9a1fdec4624e411b9f514c5a453b9a43/calls/annotate',
	CURLOPT_RETURNTRANSFER => TRUE,
	CURLOPT_ENCODING       => '',
	CURLOPT_MAXREDIRS      => 10,
	CURLOPT_TIMEOUT        => 0,
	CURLOPT_FOLLOWLOCATION => TRUE,
	CURLOPT_HTTP_VERSION   => CURL_HTTP_VERSION_1_1,
	CURLOPT_CUSTOMREQUEST  => 'POST',
	CURLOPT_POSTFIELDS     => json_encode($post_data),
	CURLOPT_HTTPHEADER     => array(
		'Authorization: Token 09f0c9f0920a3d8be5d9d04ab8a975ee2646ca82d8832162b45212e9f7e32d6742476b6a0fd0b1b77be3d86bf1a07eb20f0b345f243a968819aec47379dbff53824d4afee7ddb4e18ca2ec1a678d48fffff3be3bec89f9550dd354f555501a8309e8ed8e53352902433d2c60482a879a6a2834c6',
	),
));

$response = curl_exec($curl);

curl_close($curl);
echo $response;
