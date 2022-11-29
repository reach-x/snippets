<?php
$page_id = 1;
$max_pages = PHP_INT_MAX;

$cap_keys = array(
	"daily_conversion_cap",
	"weekly_conversion_cap",
	"monthly_conversion_cap",
	"global_conversion_cap",
	"daily_payout_cap",
	"weekly_payout_cap",
	"monthly_payout_cap",
	"global_payout_cap",
	"daily_revenue_cap",
	"weekly_revenue_cap",
	"monthly_revenue_cap",
	"global_revenue_cap",
	"daily_click_cap",
	"weekly_click_cap",
	"monthly_click_cap",
	"global_click_cap",
);

$cap_status = array();

while ($page_id < $max_pages) {
	$url_template = "https://api.eflow.team/v1/networks/offerstable?page=%s&page_size=50&order_field=name&order_direction=asc&relationship=ruleset&relationship=tracking_domain&relationship=account_manager&relationship=sales_manager";
	$url = sprintf($url_template, $page_id);
	$api_response = get_everflow_api_response($url);

	foreach ($api_response['offers'] as $offer) {
		$remaining_caps = $offer["relationship"]["remaining_caps"];

		foreach ($cap_keys as $cap_key) {
			$current_cap = $offer[$cap_key];
			$remaining_cap = $remaining_caps["remaining_{$cap_key}"];

			if ($current_cap > 0 || $remaining_cap > 0) {
				if (!isset($cap_status[$offer['network_offer_id']])) {
					$cap_status[$offer['network_offer_id']] = array();
				}

				$cap_percent = (round($remaining_cap / $current_cap, 4) * 100);

				$cap_status[$offer['network_offer_id']][] = array(
					'name'          => $offer['name'],
					'cap_key'       => $cap_key,
					'current_cap'   => $current_cap,
					'remaining_cap' => $remaining_cap,
					'cap_percent'   => $cap_percent,
					'alert'         => ($cap_percent < 5),
				);
			}
		}
	}

	$paging_info = $api_response['paging'];
	$max_pages = intval($paging_info['total_count'] / $paging_info['page_size']);
	$page_id++;
	printf("Page %d of %d\n", $page_id, $max_pages);
}

printf("%s\n", json_encode($cap_status, JSON_PRETTY_PRINT));

function get_everflow_api_response($url)
{
	$curl = curl_init();
	curl_setopt_array($curl, [
		CURLOPT_URL            => $url,
		CURLOPT_RETURNTRANSFER => TRUE,
		CURLOPT_ENCODING       => "",
		CURLOPT_MAXREDIRS      => 10,
		CURLOPT_TIMEOUT        => 30,
		CURLOPT_HTTP_VERSION   => CURL_HTTP_VERSION_1_1,
		CURLOPT_CUSTOMREQUEST  => "POST",
		CURLOPT_POSTFIELDS     => '{ "filters": { "offer_status": "active" }, "search_terms": [] }',
		CURLOPT_HTTPHEADER     => [
			"Content-Type: application/json",
			"x-eflow-api-key: 9kTVz2cHRdaeeQ5beBvujg",
		],
	]);

	$response = curl_exec($curl);
	$err = curl_error($curl);

	curl_close($curl);

	if ($err) {
		echo "cURL Error #:" . $err;
		exit;
	} else {
		$api_response = json_decode($response, TRUE);

		if (json_last_error()) {
			echo json_last_error_msg();
			exit;
		} else {
			return $api_response;
		}
	}
}