#!php


<?php

$file = "/Users/jbrahy/Projects/snippets/php/tmp/tracking_domains.json";

$json = json_decode(file_get_contents($file),TRUE);

foreach ($json['domains'] as $domain )
{
	printf("%s\n",$domain['url']);
}
