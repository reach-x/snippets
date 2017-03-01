<?php

/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 2/24/17
 * Time: 12:32
 */

$curler = new curler();

$post_data = [
	'USERNAME' => "username",
	'PASSWORD' => "password",
	'COMMAND' => "command",
];

$response = $curler->post_request($post_data);

printf("response: %s\n", response);

class curler {


	/**
	 * @param array $post_data
	 *
	 * @return array
	 */
	public function post_request($post_data = array()) {

		$url = 'http://<hostname>/post/inbound';
		$cgi_data = array_merge($post_data, $this->data['step']);

		$request = http_build_query($cgi_data);
		$response = $this->post(array(
			'method' => 'POST',
			'url' => $url,
		), $request);

		return $response;
	}

	/**
	 * @param $server
	 * @param $request
	 *
	 * @return array
	 */
	public function post($server, $request) {

		$start_time = microtime(TRUE);

		$ch = curl_init();

		if ($server['method'] == "POST") {
			curl_setopt_array($ch, array(
				CURLOPT_URL => $server['url'],
				CURLOPT_POSTFIELDS => $request,
			));
		} else {
			curl_setopt_array($ch, array(
				CURLOPT_URL => sprintf("%s?%s", $server['url'], $request),
			));
		}

		curl_setopt_array($ch, array(
			CURLOPT_POST => ($server['method'] == 'POST') ? 1 : 0,
			// i am sending post data
			CURLOPT_HTTPGET => ($server['method'] == 'GET') ? 1 : 0,
			CURLOPT_RETURNTRANSFER => TRUE,
			// return web page
			CURLOPT_HEADER => FALSE,
			// don't return headers
			CURLOPT_FOLLOWLOCATION => TRUE,
			// follow redirects
			CURLOPT_ENCODING => "",
			// handle all encodings
			CURLOPT_USERAGENT => "Reach-X/Publishing System v4.20",
			// who am i
			CURLOPT_AUTOREFERER => TRUE,
			// set referer on redirect
			CURLOPT_CONNECTTIMEOUT => 120,
			// timeout on connect
			CURLOPT_TIMEOUT => 120,
			// timeout on response
			CURLOPT_MAXREDIRS => 10,
			// stop after 10 redirects
			CURLOPT_SSL_VERIFYHOST => 0,
			// don't verify ssl
			CURLOPT_SSL_VERIFYPEER => FALSE,
			//
			CURLOPT_VERBOSE => 0,
			CURLOPT_SSLVERSION => 1,
		));

		$content = curl_exec($ch);
		$error_no = curl_errno($ch);
		$error_message = curl_error($ch);
		$header = print_r(curl_getinfo($ch), TRUE);
		$content_type = curl_getinfo($ch, CURLINFO_CONTENT_TYPE);

		curl_close($ch);

		return array(
			'server' => $server,
			'request' => $request,
			'header' => $header,
			'content_type' => $content_type,
			'content' => $content,
			'error_no' => $error_no,
			'error_message' => $error_message,
			'response_time' => (microtime(TRUE) - $start_time),
		);
	}
}