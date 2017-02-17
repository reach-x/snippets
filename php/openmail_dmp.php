<?php


$email = $argv[1];

$parameters = array(
	"key" => "9vZbim8rK48xPhxD9kIC",
	"user_info" => sprintf("'[{\"email\":\"%s\"}]'",$email),
);

$post_data = http_build_query($parameters);

$ch = curl_init();
curl_setopt_array($ch,array(
		CURLOPT_URL => "https://dmp.openmail.com/v1/user_info",
		CURLOPT_POST => TRUE,
		CURLOPT_RETURNTRANSFER => TRUE,
		CURLOPT_VERBOSE => TRUE,
		CURLOPT_POSTFIELDS => $post_data,
		CURLOPT_HTTPHEADER => array('Content-Type: multipart/form-data; Content-Length: ' . strlen($post_data)),
	) 
);

print_r(array(
	'parameters' => $parameters,
	'post_data' => $post_data,
));
printf("CURL_OUTPUT: %s\n",curl_exec($ch));

//curl -i https://dmp.openmail.com/v1/user_info \ 
//       -d key='9vZbim8rK48xPhxD9klC' \
//       -d user_info='[{"has_pet": true, "gender": "male", "email": "foo@bar.com"}]'

