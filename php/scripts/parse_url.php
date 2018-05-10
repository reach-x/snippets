<?php

$url = "http://www.gobankcards.com/tr/136?first_name=Johnny&last_name=%%LAST_NAME%%&email_address=%%EMAIL_ADDRESS%%&phone_number=%%HOME_PHONE%%&zip_code=%%ZIP_CODE%%&city=%%CITY%%&state=%%STATE%%";



$url_parts = parse_url($url);

print_r($url_parts);

$query = array();

parse_str($url_parts['query'],$query);

foreach ($query as $key => $value){
	$tokens = substr_count($value,"%");
	if ($tokens == 4){
		unset($query[$key]);
		printf("unsetting $key\n");
	}
	printf("%s: %s\n",$key,$value);
}

print_r($query);

