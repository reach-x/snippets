<?php
$string = $argv[1];
$start = $argv[2];
$length = $argv[3];

print_r(array(
	'$string' => $string,
	'$start' => $start,
	'$length' => $length,
	'substr' => substr($string, $start, $length),
));


///**
// * Created by PhpStorm.
// * User: jbrahy
// * Date: 3/13/17
// * Time: 13:08
// */
//
//
//$response = <<<EOH
//HTTP/1.1 201 CREATED
//Server: nginx/1.8.1
//Date: Mon, 13 Mar 2017 20:03:52 GMT
//Content-Type: application/json
//Transfer-Encoding: chunked
//Connection: keep-alive Vary: Accept, Cookie
//Allow: GET, POST, HEAD, OPTIONS
//{
//	"id": 92130949,
//	"contact": 92130949,
//	"phone_number": 12532505565,
//	"title": null,
//	"first_name": "Jeanne",
//	"middle_name": null,
//	"last_name": "Patterson",
//	"company_name": null,
//	"email_address": "jeannep414@msn.com",
//	"address": null,
//	"address2": null,
//	"city": "Brem",
//	"state": "WA",
//	"county": null,
//	"zip_code": "98312",
//	"country": null,
//	"mobile_number": "2532505565",
//	"alternate_number": null,
//	"date_created": "2017-03-13T20:03:52.829317Z",
//	"dial_history": null,
//	"queue": "fast"
//}
//EOH;
//
//$header_length = 216;
//
//printf("HEADER: %s\n",substr($response,0,$header_length));
//printf("BODY: %s\n",substr($response,$header_length));
