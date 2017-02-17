<?php

$cipher = constant("MCRYPT_BLOWFISH");
$key = "openbsd";
$modes = array("ecb", "cbc", "cfb", "ofb", "nofb", "stream","ctr");
$iv = 1234;

$plain_text = $argv[1];

$cipher_text = mcrypt_encrypt($cipher,$key,$plain_text,$mode,$iv);

foreach ($modes as $mode){
print_r(array(
	"mode" => $mode,
	"argv[1]" => $argv[1],
	"mcrypt_encrypt" => mcrypt_encrypt($cipher,$key,$plain_text,$mode,$iv),
	"mcrypt_decrypt" => mcrypt_decrypt($cipher,$key,$cipher_text,$mode,$iv),
));
}


