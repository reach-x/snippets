<?php

$sites = file("sites.txt");

foreach ($sites as $site){
	$site = trim($site);
	$contents = file_get_contents(sprintf("http://%s",$site));
	printf("%s\n",$site);
}
