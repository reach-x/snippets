<?php

$domains = array(
	"www.google.com",
	"www.google.co.uk",
	"www.google.com",
	"google.com",
	"google.co.uk",
);

foreach ($domains as $domain){
	print "\n" . $domain . ": " . get_domain($domain) . "\n";
}


function get_domain ($domain) {
	$subdomains = array("/develop./","/local./","/www./","/dev./");
	$replacements = array_fill(0,count($subdomains),"");
	$domain = preg_replace($subdomains,$replacements,$domain);
	$parts = substr_count(".",$domain);
	return join(".",array_slice(explode(".",$domain),$parts));
}
