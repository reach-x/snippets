<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 11/21/18
 * Time: 15:35
 */

$sql = <<<EOQ
SELECT tbl_comment.*,tbl_like_unlike.like_unlike 
FROM tbl_comment 
	LEFT JOIN tbl_like_unlike ON tbl_comment.comment_id = tbl_like_unlike.comment_id AND member_id = " . $memberId . " 
ORDER BY tbl_like_unlike.like_unlike DESC, tbl_comment.date DESC
EOQ;


function pull_from_api() {

	$page_limit = 1000;

	$top_limit = //$this->get_count() + 1000;

	$campaigns = array();
	$offset = 0;

	while ($offset < $top_limit) {

		printf("Pulling %d to %d\n", $offset, ($offset + $page_limit));

		$url = "http://api.1318amethyst.com/api/v1.1/campaigns?all_details=1&offset={$offset}&limit={$page_limit}";

		$ch = curl_init($url);
		curl_setopt($ch, CURLOPT_HTTPGET, TRUE);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
		curl_setopt($ch, CURLOPT_FOLLOWLOCATION, TRUE);
		curl_setopt($ch, CURLOPT_HTTPHEADER, array(
			"auth_token:6d4888962feba3c669176b81edacb185",
			"accept:application/json",
		));

		$raw_response = curl_exec($ch);

		if (curl_errno($ch) > 0) {
			print_r(array(
				'error' => curl_error($ch),
				'response' => $raw_response,
			));
		}
		$campaigns = //$this->append_campaigns($campaigns, json_decode($raw_response));

		curl_close($ch);

		$offset += $page_limit;
	}

	printf("Pulled %d campaigns\n", count($campaigns));

	return $campaigns;

}



function append_campaigns($existing_campaigns, $more_campaigns) {

	foreach ($more_campaigns->campaigns as $campaigns) {
		$existing_campaigns[] = $campaigns;
	}

	return $existing_campaigns;
}