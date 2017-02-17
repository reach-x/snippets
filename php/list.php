<?php

$contents = file_get_contents("list.json");

$structure = json_decode($contents);

foreach ($structure as $element) {
	printf("%s\n\n", $element->name);
}

