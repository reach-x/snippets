<?php

$source_domains = array(
	"www.dermanueye.com/",
	"surveys.contact-101.com/index.php?sid=76060716875&scode=SURV",
	"https://pinklaptop4free.co.uk",
	"http://www.buy-max-garcinia-cambogia.com/",
	"Customer expressed interested in the following products: Zonecore SuperColon",
);

foreach ($source_domains as $source_domain){
	printf("START: %s\nRESULT: %s\n\n",$source_domain, extract_domain(array('source_domain' => $source_domain)));
}

function extract_domain($owner_post) {

            $source_domain = $owner_post['source_domain'];
		
            printf("processing %s\n",$owner_post['source_domain']);

	    // first remove any http(s?)
	    $source_domain = str_replace(array("http://","https://"),"",$source_domain);

	    // remove anything after a question mark
	    $url_parts = explode("?",$source_domain);
	    $source_domain = $url_parts[0];

	    if (preg_match("~^https?://~",$source_domain)){
	    } else {
	    	$source_domain = sprintf("http://%s",$source_domain);
	    }

            $pieces = parse_url($source_domain);
            $domain = isset( $pieces['host'] ) ? $pieces['host'] : $source_domain;

            // strip everything from the url so it's just the domain name
            if (preg_match('/(?P<domain>[a-z0-9][a-z0-9\-]{1,63}\.[a-z\.]{2,6})$/i', $domain, $regs)) {
                $source_domain = $regs['domain'];
            
	    } else {
		$source_domain = $owner_post['source_domain'];
            }


        return $source_domain;
}

