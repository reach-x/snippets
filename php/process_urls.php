<?php

$urls = file("/Users/jbrahy/tmp/urls");

foreach($urls as $url){
	parse_str($url,$params);
//	print_r($params);
	printf("%s %s\n",$params['email'],$params['dob']);
}
