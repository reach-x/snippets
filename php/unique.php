<?php


$first_half = sprintf("%s-%s",uniqid(TRUE),microtime(TRUE));
$cookie_key = sprintf("%s-%s",$first_half,md5($first_half));

printf("cookie_key: %s\n",$cookie_key);
printf("reverse: %s\n", gen_cookie_key());


function gen_cookie_key() {

		$first_half = sprintf("%s-%s", uniqid(TRUE), microtime(TRUE));
		$cookie_key = sprintf("%s-%s", $first_half, md5($first_half));

		return join("",array_reverse(preg_split('//',$cookie_key)));
	}
