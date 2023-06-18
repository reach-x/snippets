<?php

$page = array(
	'page1.php',
	'page2.php',
	'page3.php',
);

print file_get_contents($page[rand(0, count($page) - 1)]);

printf("loading %s\n",$page[rand(0, count($page) - 1)]);
