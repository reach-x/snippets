<?php
$votes = 100;

while($votes > 0){
	printf("vote: %d\n",$votes);
	$url = sprintf("http://www.10best.com/awards/travel/best-new-travel-app/just-ahead/?c=%s",rand());
	$contents = get_contents($url);
	$matches = array();
	preg_match_all('/voteKey" value="(.*)" \/>/', $contents,$matches);
	print_r(array('matches'=>$matches[1][0]));
	$url = sprintf("http://www.10best.com/common/ajax/vote.php?voteKey=%s&email=&c=%s",$matches[1][0],rand());
	get_contents($url);
	$votes--;
}

function get_contents($url){
	$ch = curl_init($url);
	curl_setopt($ch, CURLOPT_COOKIESESSION, TRUE);
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
	curl_setopt($ch, CURLOPT_FRESH_CONNECT, TRUE);
	$output = curl_exec($ch);
	curl_close($ch);
	return $output;
}
