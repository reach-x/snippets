#!/usr/bin/php
<?php

if (!isset($_SERVER['REMOTE_ADDR'])) {
	$_SERVER['REMOTE_ADDR'] = "127.0.0.1";
}

ini_set("memory_limit", "4G");

$scrub_date = date('Y-m-d 00:00:00');

$test = FALSE;

// bring in central configuration file
include_once("/var/www/vhosts/inboxmediacorp.com/subdomains/admin/httpdocs/Includes/configure.php");
include_once("/var/www/vhosts/inboxmediacorp.com/subdomains/admin/httpdocs/Classes/class.imbk5.php");

$imb = new imbk5();

// Connect to the Admin Database.
$admin_database = new mysqli(DB_SERVER, DB_SERVER_USERNAME, DB_SERVER_PASSWORD, DB_ADMIN_DATABASE) or die("Connection Error for admin DB: " . $admin_database->error);

// LeadData
$lead_database = new mysqli(DB_SERVER_2, DB_SERVER_USERNAME_2, DB_SERVER_PASSWORD_2, DB_ADMIN_DATABASE_2) or die("Connection Error: " . $lead_database->errors);

// Our counters for creating a LOG entry
$tracking_data = build_tracking_object();
$import_date_sql = generate_datetime();

$get_new_leads_sql = <<<SQL
	SELECT
		VID,
		AID,
		LID,
		SID,
		Date,
		FirstName,
		LastName,
		Address,
		City,
		State,
		Zip,
		PrimaryPhone,
		IPAddress,
		Website,
		Email,
		Status
	FROM
		LEADS_INCOMING 
SQL;

$new_leads_query = $lead_database->query($get_new_leads_sql) or die ($lead_database->error);

$index = 0;

$vid_count = array();
$vendor_data = array();
$email_address_array = array();

while ($lead = $new_leads_query->fetch_array()) {
	$lead_email_address = strtolower(filter_var(trim($lead['Email']), FILTER_SANITIZE_EMAIL));

	list($_, $lead_email_domain) = explode('@', $lead_email_address, 2);

	if (endsWith($lead_email_address, '.gov')) {
		$lead['Status'] = '2';
	} else {

		$lead_email_domain_escaped = $admin_database->escape_string($lead_email_domain);
		$sql = sprintf("SELECT COUNT(*) as count FROM UNSUB_QUEUE WHERE type = 'domain' AND data = '%s'", $lead_email_domain_escaped);
		$email_domain_unsubscribed_count_result = $admin_database->query($sql) or die (sprintf("SQL: %s\nERROR: %s\n", $sql, $admin_database->error));
		$email_domain_unsubscribed_count_row = $email_domain_unsubscribed_count_result->fetch_assoc();

		if ($email_domain_unsubscribed_count_row['count'] > 0) {

			$lead['Status'] = '2';

		} else {

			$lead_email_address_escaped = $admin_database->escape_string($lead_email_address);
			$sql = "SELECT COUNT(*) as count FROM UNSUB_EMAILS WHERE email = '{$lead_email_address_escaped}'";
			$lead_email_address_lookup_result = $admin_database->query($sql) or die (sprintf("SQL: %s\nERROR: %s\n", $sql, $admin_database->error));

			$lead_email_address_lookup_row = $lead_email_address_lookup_result->fetch_assoc();

			if ($lead_email_address_lookup_row['count'] > 0) {
				$lead['Status'] = '2';
			}
		}
	}

	// What is the AID of this record? (list id)
	$current_list_AID = $lead['AID'];
	$current_VID = $lead['VID'];

	$email_address_escaped = $lead_database->escape_string($lead_email_address);
	$lead_email_lookup_sql = sprintf("SELECT * FROM LEADS_LEGACY WHERE Email = '%s'", $email_address_escaped);

	$lead_email_lookup_result = $lead_database->query($lead_email_lookup_sql) or die ($lead_database->error);

	$lead_email_lookup_row_count = $lead_email_lookup_result->num_rows;

	$is_global_unique = $lead_email_lookup_row_count <= 0 ? '1' : '0';

	// Is list unique calculation
	$is_list_unique = TRUE;

	while ($duplicate_email_address_row = $lead_email_lookup_result->fetch_assoc()) {
		if ($duplicate_email_address_row['VID'] == $current_VID) {
			$is_list_unique = FALSE;
			break;
		}
	}

	$email_type = get_email_domain_subset($lead_email_address);

	// Copying this variable
	$status = $lead['Status'];

	// Increment our counters for our LOG
	if (!isset($tracking_data['all_records'][$current_list_AID])) {
		$tracking_data['all_records'][$current_list_AID] = 1;
	} else {
		$tracking_data['all_records'][$current_list_AID]++;
	}

	if ($status == '2') {

		if (!isset($tracking_data['all_unsubscribers'][$current_list_AID])) {
			$tracking_data['all_unsubscribers'][$current_list_AID] = 1;
		} else {
			$tracking_data['all_unsubscribers'][$current_list_AID]++;
		}
	}

	if ($is_global_unique == '1') {

		if (!isset($tracking_data['all_unique_records'][$current_list_AID])) {
			$tracking_data['all_unique_records'][$current_list_AID] = 1;
		} else {
			$tracking_data['all_unique_records'][$current_list_AID]++;
		}

		if ($status == '2') {
			if (!isset($tracking_data['unique_unsubscribers'][$current_list_AID])) {
				$tracking_data['unique_unsubscribers'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_unsubscribers'][$current_list_AID]++;
			}
		}
	}

	if ($is_list_unique) {

		if (!isset($tracking_data['unique_record_lists'][$current_list_AID])) {
			$tracking_data['unique_record_lists'][$current_list_AID] = 1;
		} else {
			$tracking_data['unique_record_lists'][$current_list_AID]++;
		}

		if ($status == '2') {
			if (!isset($tracking_data['unique_unsubscribe_lists'][$current_list_AID])) {
				$tracking_data['unique_unsubscribe_lists'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_unsubscribe_lists'][$current_list_AID]++;
			}
		}
	}

	if ($email_type == 'aol') {

		if (!isset($tracking_data['all_aol'][$current_list_AID])) {
			$tracking_data['all_aol'][$current_list_AID] = 1;
		} else {
			$tracking_data['all_aol'][$current_list_AID]++;
		}

		if ($is_global_unique == '1') {
			if (!isset($tracking_data['unique_aol'][$current_list_AID])) {
				$tracking_data['unique_aol'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_aol'][$current_list_AID]++;
			}
		}

		if ($is_list_unique) {
			if (!isset($tracking_data['unique_aol_lists'][$current_list_AID])) {
				$tracking_data['unique_aol_lists'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_aol_lists'][$current_list_AID]++;
			}
		}

	} else if ($email_type == 'msn') {

		if (!isset($tracking_data['all_msn'][$current_list_AID])) {
			$tracking_data['all_msn'][$current_list_AID] = 1;
		} else {
			$tracking_data['all_msn'][$current_list_AID]++;
		}

		if ($is_global_unique == '1') {
			if (!isset($tracking_data['unique_msn'][$current_list_AID])) {
				$tracking_data['unique_msn'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_msn'][$current_list_AID]++;
			}
		}

		if ($is_list_unique) {
			if (!isset($tracking_data['unique_msn_lists'][$current_list_AID])) {
				$tracking_data['unique_msn_lists'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_msn_lists'][$current_list_AID]++;
			}
		}

	} else if ($email_type == 'yahoo') {

		if (!isset($tracking_data['all_yahoo'][$current_list_AID])) {
			$tracking_data['all_yahoo'][$current_list_AID] = 1;
		} else {
			$tracking_data['all_yahoo'][$current_list_AID]++;
		}

		if ($is_global_unique == '1') {
			if (!isset($tracking_data['unique_yahoo'][$current_list_AID])) {
				$tracking_data['unique_yahoo'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_yahoo'][$current_list_AID]++;
			}
		}

		if ($is_list_unique) {
			if (!isset($tracking_data['unique_yahoo_lists'][$current_list_AID])) {
				$tracking_data['unique_yahoo_lists'][$current_list_AID] = 1;

			} else {
				$tracking_data['unique_yahoo_lists'][$current_list_AID]++;
			}
		}

	} else if ($email_type == 'google') {

		if (!isset($tracking_data['all_google'][$current_list_AID])) {
			$tracking_data['all_google'][$current_list_AID] = 1;
		} else {
			$tracking_data['all_google'][$current_list_AID]++;
		}

		if ($is_global_unique == '1') {
			if (!isset($tracking_data['unique_google'][$current_list_AID])) {
				$tracking_data['unique_google'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_google'][$current_list_AID]++;
			}
		}

		if ($is_list_unique) {
			if (!isset($tracking_data['unique_google_lists'][$current_list_AID])) {
				$tracking_data['unique_google_lists'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_google_lists'][$current_list_AID]++;
			}
		}

	} else {

		if (!isset($tracking_data['all_general'][$current_list_AID])) {
			$tracking_data['all_general'][$current_list_AID] = 1;
		} else {
			$tracking_data['all_general'][$current_list_AID]++;
		}

		if ($is_global_unique == '1') {
			if (!isset($tracking_data['unique_general'][$current_list_AID])) {
				$tracking_data['unique_general'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_general'][$current_list_AID]++;
			}
		}

		if ($is_list_unique) {
			if (!isset($tracking_data['unique_general_lists'][$current_list_AID])) {
				$tracking_data['unique_general_lists'][$current_list_AID] = 1;
			} else {
				$tracking_data['unique_general_lists'][$current_list_AID]++;
			}
		}
	}

	$lead_email_address = strtolower($lead['Email']);
	$lead_vid = $lead_database->escape_string($lead['VID']);
	$lead_aid = $lead_database->escape_string($lead['AID']);
	$lead_lid = $lead_database->escape_string($lead['LID']);
	$lead_sid = $lead_database->escape_string($lead['SID']);
	$lead_date = $lead_database->escape_string($lead['Date']);
	$lead_first_name = $lead_database->escape_string($lead['FirstName']);
	$lead_last_name = $lead_database->escape_string($lead['LastName']);
	$lead_address = $lead_database->escape_string($lead['Address']);
	$lead_city = $lead_database->escape_string($lead['City']);
	$lead_state = $lead_database->escape_string($lead['State']);
	$lead_zipcode = $lead_database->escape_string($lead['Zip']);
	$lead_phone = $lead_database->escape_string($lead['PrimaryPhone']);
	$lead_ip_address = $lead_database->escape_string($lead['IPAddress']);
	$lead_website = $lead_database->escape_string($lead['Website']);
	$lead_status = $lead_database->escape_string($lead['Status']);
	$is_global_unique = $is_global_unique ? 1 : 0;
	$is_list_unique = $is_list_unique ? 1 : 0;

	$insert_into_leads_legacy_sql_template = <<<SQL
INSERT IGNORE INTO LEADS_LEGACY
	(`VID`,`AID`,`LID`,`SID`,`Date`,`FirstName`,`LastName`,`Address`,`City`,`State`,`Zip`,`PrimaryPhone`,`IPAddress`,`Website`,`Email`,`Status`, `Unique1`, `Unique2`)
VALUES
	('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s', '%s', '%s')
SQL;

	$insert_into_leads_legacy_sql = sprintf($insert_into_leads_legacy_sql_template, $lead_vid, $lead_aid, $lead_lid, $lead_sid, $lead_date, $lead_first_name, $lead_last_name, $lead_address, $lead_city, $lead_state, $lead_zipcode, $lead_phone, $lead_ip_address, $lead_website, $lead_email_address, $lead_status, $is_global_unique, $is_list_unique);

	if ($test === FALSE) {
		$insert = $lead_database->query($insert_into_leads_legacy_sql);

		if ($lead_database->errno > 0) {
			printf("ERROR INSERTING: %s\n%s\n", $insert_into_leads_legacy_sql, $lead_database->error);
			exit(1);
		} else {
			//if insert was successful, let's delete that record now .
			$sql = sprintf("DELETE FROM LEADS_INCOMING WHERE VID='%s' AND Email='%s'\n", $lead_vid, $lead_email_address);
			$lead_database->query($sql);
		}


		$tracking_data['counter_good']++;

		// now we check for duplicates using an array - very fast - but can consume a lot of memory
		if ($insert) {

			if (isset($email_address_array[$lead['Email']]) && $email_address_array[$lead['Email']] > 0) {
				$tracking_data['counter_duplicates']++;
			}

			if (!isset($email_address_array[$lead['Email']])) {
				$email_address_array[$lead['Email']] = 1;
			} else {
				$email_address_array[$lead['Email']]++;
			}
		}

		if ($tracking_data['counter_records'] % 100 == 0) {
			printf("record #: %d %s\n", $tracking_data['counter_records'], $lead['Email']);
		}

		if (!isset($vid_count[$lead['VID']])) {
			$vid_count[$lead['VID']] = 1;
		} else {
			$vid_count[$lead['VID']]++;
		}

		if (!isset($aid_count[$lead['AID']])) {
			$aid_count[$lead['AID']] = 1;
		} else {
			$aid_count[$lead['AID']]++;
		}

		$tracking_data['counter_records']++;
	}
}

foreach (array_keys($tracking_data['all_records']) as $list_id) {
	$list_id_escaped = $admin_database->escape_string($list_id);

	$current_records_all = $tracking_data['all_records'][$list_id];
	$current_records_aol = isset($tracking_data['all_aol'][$list_id]) ? $tracking_data['all_aol'][$list_id] : 0;
	$current_general_all = isset($tracking_data['all_general'][$list_id]) ? $tracking_data['all_general'][$list_id] : 0;
	$current_msn_all = isset($tracking_data['all_msn'][$list_id]) ? $tracking_data['all_msn'][$list_id] : 0;
	$current_yahoo_all = isset($tracking_data['all_yahoo'][$list_id]) ? $tracking_data['all_yahoo'][$list_id] : 0;
	$current_google_all = isset($tracking_data['all_google'][$list_id]) ? $tracking_data['all_google'][$list_id] : 0;
	$current_unsubscribers_all = isset($tracking_data['all_unsubscribers'][$list_id]) ? $tracking_data['all_unsubscribers'][$list_id] : 0;
	$current_unique_records = isset($tracking_data['all_unique_records'][$list_id]) ? $tracking_data['all_unique_records'][$list_id] : 0;
	$current_tracking_data['unique_aol'] = isset($tracking_data['unique_aol'][$list_id]) ? $tracking_data['unique_aol'][$list_id] : 0;
	$current_tracking_data['unique_general'] = isset($tracking_data['unique_general'][$list_id]) ? $tracking_data['unique_general'][$list_id] : 0;
	$current_tracking_data['unique_msn'] = isset($tracking_data['unique_msn'][$list_id]) ? $tracking_data['unique_msn'][$list_id] : 0;
	$current_tracking_data['unique_yahoo'] = isset($tracking_data['unique_yahoo'][$list_id]) ? $tracking_data['unique_yahoo'][$list_id] : 0;
	$current_tracking_data['unique_google'] = isset($tracking_data['unique_google'][$list_id]) ? $tracking_data['unique_google'][$list_id] : 0;
	$current_tracking_data['unique_unsubscribers'] = isset($tracking_data['unique_unsubscribers'][$list_id]) ? $tracking_data['unique_unsubscribers'][$list_id] : 0;
	$current_list_unique_all = isset($tracking_data['unique_record_lists'][$list_id]) ? $tracking_data['unique_record_lists'][$list_id] : 0;
	$current_list_tracking_data['unique_aol'] = isset($tracking_data['unique_aol_lists'][$list_id]) ? $tracking_data['unique_aol_lists'][$list_id] : 0;
	$current_list_tracking_data['unique_general'] = isset($tracking_data['unique_general_lists'][$list_id]) ? $tracking_data['unique_general_lists'][$list_id] : 0;
	$current_list_tracking_data['unique_msn'] = isset($tracking_data['unique_msn_lists'][$list_id]) ? $tracking_data['unique_msn_lists'][$list_id] : 0;
	$current_list_tracking_data['unique_yahoo'] = isset($tracking_data['unique_yahoo_lists'][$list_id]) ? $tracking_data['unique_yahoo_lists'][$list_id] : 0;
	$current_list_tracking_data['unique_google'] = isset($tracking_data['unique_google_lists'][$list_id]) ? $tracking_data['unique_google_lists'][$list_id] : 0;
	$current_list_tracking_data['unique_unsubscribers'] = isset($tracking_data['unique_unsubscribe_lists'][$list_id]) ? $tracking_data['unique_unsubscribe_lists'][$list_id] : 0;


	$insert_into_import_list_logs_sql = <<<SQL
	INSERT INTO IMPORT_LIST_LOGS
	(list_id, import_date, records_all, aol_all, gen_all, msn_all, yahoo_all, google_all, unsub_all, records_unique, aol_unique, gen_unique, msn_unique, yahoo_unique, google_unique, unsub_unique, records_list_unique, aol_list_unique, gen_list_unique, msn_list_unique, yahoo_list_unique, google_list_unique, unsub_list_unique)
	VALUES ('{$list_id_escaped}', '{$import_date_sql}', '{$current_records_all}', '{$current_records_aol}', '{$current_general_all}', '{$current_msn_all}', '{$current_yahoo_all}', '{$current_google_all}', '{$current_unsubscribers_all}', '{$current_unique_records}', '{$current_tracking_data['unique_aol']}', '{$current_tracking_data['unique_general']}', '{$current_tracking_data['unique_msn']}', '{$current_tracking_data['unique_yahoo']}', '{$current_tracking_data['unique_google']}', '{$current_tracking_data['unique_unsubscribers']}', '{$current_list_unique_all}', '{$current_list_tracking_data['unique_aol']}', '{$current_list_tracking_data['unique_general']}', '{$current_list_tracking_data['unique_msn']}', '{$current_list_tracking_data['unique_yahoo']}', '{$current_list_tracking_data['unique_google']}', '{$current_list_tracking_data['unique_unsubscribers']}')
SQL;

	$insert = $admin_database->query($insert_into_import_list_logs_sql) or die ($admin_database->error);

	if ($admin_database->errno > 0) {
		printf("ERROR INSERTING: %s\n%s\n", $insert_into_import_list_logs_sql, $admin_database->error);
	}

}

// mail this report out!
if ($tracking_data['counter_records'] > 0) {

	$report_date = $scrub_date = date('Y-m-d');
	$email_array = array(
		'tam@inboxmediacorp.com',
		'anthony@thinkasdf.com',
		'jbrahy@thinkasdf.com',
	);

	$subject = "Lead Backup Stats for " . $report_date;

	$tracking_data['counter_good'] = number_format($tracking_data['counter_good']);
	$tracking_data['counter_duplicates'] = number_format($tracking_data['counter_duplicates']);
	$tracking_data['counter_bad'] = number_format($tracking_data['counter_bad']);
	$tracking_data['counter_records'] = number_format($tracking_data['counter_records']);

	$body = $subject . "\n";
	$body .= "=====================================================================\n";
	$body .= "Inserts: " . $tracking_data['counter_good'] . "\n";
	$body .= "Duplicates: " . $tracking_data['counter_duplicates'] . "\n";
	$body .= "Total Records: " . $tracking_data['counter_records'] . "\n\n";

	$unique_vid_ids = array_unique($vid_count);

	$body .= "Vendor Lead Breakdown\n";
	$body .= "=====================================================================\n";

	$vendor_name_array = array();
	$vid_array = array();

	foreach ($unique_vid_ids as $vid => $vcount) {

		$vcount = number_format($vcount);

		// query the LISTS table and get the vendor_name from the vid
		$vendor_name = $imb->get_list_name_by_vid($vid);
		$vendor_name_array[$vendor_name] = $vcount;

		$vid_array[$vendor_name] = $vid;

		// get the unique lead count by email key
		$vendor_unique_emails_sql = <<<SQL
			SELECT
				COUNT(DISTINCT(Email)) AS vendor_uniques
			FROM
				LEADS_INCOMING
			WHERE
				VID = '{$vid}'
SQL;

		$vendor_unique_emails_result = $lead_database->query($vendor_unique_emails_sql) or printf("%s\n", $lead_database->error);;

		if ($lead_database->errno > 0) {
			printf("ERROR INSERTING: %s\n%s\n", $vendor_unique_emails_sql, $lead_database->error);
		}

		$vendor_unique_emails_arr = $vendor_unique_emails_result->fetch_assoc();
		$uniques_count = $vendor_unique_emails_arr['vendor_uniques'];

		$vendor_unique_emails_result->free();

		// save the unique leads per vendor
		$vendor_unique_array[$vid]['uniques_count'] = number_format($uniques_count);

		$vendor_total_emails_sql = <<<SQL
			SELECT
				COUNT(*) AS vendor_total
			FROM
				LEADS_INCOMING
			WHERE
				VID = '{$vid}'
SQL;

		$vendor_total_emails_result = $lead_database->query($vendor_total_emails_sql) or printf("%s\n", $lead_database->error);

		if ($lead_database->errno > 0) {
			printf("ERROR INSERTING: %s\n%s\n", $vendor_total_emails_sql, $lead_database->error);
		}

		$vendor_total_emails_arr = $vendor_total_emails_result->fetch_assoc();
		$vendor_total_count = $vendor_total_emails_arr['vendor_total'];
		$vendor_total_emails_result->free();

		$duplicates_count = $vendor_total_count - $uniques_count;

		// save the unique leads per vendor
		$vendor_duplicates_array[$vid]['duplicates_count'] = $duplicates_count;
	}

	ksort($vendor_name_array);

	foreach ($vendor_name_array as $vendor_name => $vcount) {

		$vid = $vid_array[$vendor_name];
		$uniques_count = $vendor_unique_array[$vid]['uniques_count'];
		$duplicates_count = $vendor_duplicates_array[$vid]['duplicates_count'];

		$body .= "\n" . $vendor_name . ": [" . $vid . "] " . $vcount . " leads | " . $uniques_count . " uniques | " . $duplicates_count . " duplicates\n";
	}

	$headers = 'From: reporting@inboxmediacorp.com' . "\r\n" . 'Reply-To: reporting@inboxmediacorp.com' . "\r\n" . 'X-Mailer: PHP/' . phpversion();

	printf("\n%s\n", $body);

	foreach ($email_array as $email) {
		mail($email, $subject, $body, $headers);
	}
} else {
	printf("Counter was zero\n");
}

$lead_database->close();
$admin_database->close();

echo "done processing leads\n";
exit;


function get_email_domain_subset($email_address) {

	$valid_email_domains = array(
		'aol' => array('aol.com'),
		'msn' => array(
			'msn.com',
			'live.com',
			'hotmail.com',
		),
		'yahoo' => array('yahoo.com'),
		'google' => array(
			'gmail.com',
			'google.com',
		),
	);

	if (strstr($email_address, '@') === FALSE) {
		return 'gen';
	}

	list($_, $email_domain) = explode('@', $email_address, 2);
	$email_domain = strtolower($email_domain);

	foreach ($valid_email_domains as $email_type => $email_domains) {
		foreach ($email_domains as $check_email_domain) {
			if (strtolower($check_email_domain) == $email_domain) {
				return $email_type;
			}
		}
	}

	return 'gen';
}

function endsWith($haystack, $needle) {

	return $needle === "" || (($temp = strlen($haystack) - strlen($needle)) >= 0 && strpos($haystack, $needle, $temp) !== FALSE);
}


function generate_datetime($timestamp = NULL) {

	if ($timestamp === NULL) {
		$timestamp = time();
	}

	return date('Y-m-d H:i:s', $timestamp);
}

function build_tracking_object() {

	return array(
		'all_records' => array(),
		'all_aol' => array(),
		'all_general' => array(),
		'all_msn' => array(),
		'all_yahoo' => array(),
		'all_hotmail' => array(),
		'all_google' => array(),
		'all_unsubscribers' => array(),
		'all_unique_records' => array(),
		'unique_aol' => array(),
		'unique_general' => array(),
		'unique_msn' => array(),
		'unique_yahoo' => array(),
		'unique_hotmail' => array(),
		'unique_google' => array(),
		'unique_unsubscribers' => array(),
		'unique_record_lists' => array(),
		'unique_aol_lists' => array(),
		'unique_general_lists' => array(),
		'unique_msn_lists' => array(),
		'unique_yahoo_lists' => array(),
		'unique_hotmail_lists' => array(),
		'unique_google_lists' => array(),
		'unique_unsubscribe_lists' => array(),
		'counter_bad' => 0,
		'counter_good' => 0,
		'counter_records' => 0,
		'counter_duplicates' => 0,
	);
}
