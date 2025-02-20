<?php

print_r(array(
	'argv' => $argv,
	'source_domain' => extract_domain($argv[1]),
));



	function extract_domain($url) {

		// first remove any http(s?)
		$domain = str_replace(array(
			"http://",
			"https://",
		), "", $url);

		// remove anything after a question mark
		$url_parts = explode("?", $domain);
		$domain = $url_parts[0];

		if (preg_match("~^https?://~", $domain)) {
		} else {
			$domain = sprintf("http://%s", $domain);
		}

		$pieces = parse_url($domain);
		$domain = isset($pieces['host']) ? $pieces['host'] : $domain;

		// strip everything from the url so it's just the domain name
		if (preg_match('/(?P<domain>[a-z0-9][a-z0-9\-]{1,63}\.[a-z\.]{2,6})$/i', $domain, $regs)) {
			$domain = $regs['domain'];

		} else {
			$domain = $url;
		}

		return $domain;
	}

