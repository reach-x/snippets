<?php

$post_data = array(
	"ZIP_CODE" => "123456789",
	"PHONE_NUMBER" => "13608600389"
);

//printf("%s\n",substr($post_data['ZIP_CODE'], 0, 5));
$length = strlen($post_data['PHONE_NUMBER']);
printf("%s\n",substr($post_data['PHONE_NUMBER'], $length-10, 10));
