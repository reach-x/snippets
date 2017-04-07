<?php

$response = "this is the midbar and this is not.";

$response = preg_replace_callback('/midbar/', function() {

	return "midbar-" . rand();
}, $response);


printf("response: %s\n", $response);
