<?php

printf("%s\n", rand(100000, 999999));
printf("%s\n", rand(100000, 999999,true));

/*
		$responses = [
			'accept',
			'accept',
			'accept',
			'accept',
			'accept',
			'accept',
			'accept',
			'accept',
			'accept',
			'accept',
			'accept',
			'duplicate',
			'reject',
			'error',
		];

		$response = [
			'result' => $responses[rand(0, count($responses) - 1)],
		];

		printf("%s\n",json_encode($response));
*/
