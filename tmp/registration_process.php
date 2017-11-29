<?php
if (!$_POST) {
	exit;
}
$firstName = $_POST['firstName'];
$lastName = $_POST['lastName'];
$email = $_POST['email'];
$optIn = $_POST['optIn'];

if (trim($firstName) == '') {
	echo '<div class="error_message">Please enter a First Name</div>';
	exit();
} elseif (trim($lastName) == '') {
	echo '<div class="error_message">Please enter a Last Name</div>';
	exit();
} elseif (trim($email) == '') {
	echo '<div class="error_message">Please enter a Email Address</div>';
	exit();
} else if (!isEmail($email)) {
	echo '<div class="error_message">Please enter a valid Email Address.</div>';
	exit();
}

if ($error == '') {

	if (get_magic_quotes_gpc()) {
		$comments = stripslashes($comments);
	}

	require_once('db_functions.php');
	require_once('detect_functions.php');

	function checkEmail($email) {

		if (!preg_match("/^([a-zA-Z0-9])+([a-zA-Z0-9\._-])*@([a-zA-Z0-9_-])+([a-zA-Z0-9\._-]+)*.([a-zA-Z0-9_-])+([a-zA-Z0-9\._-]+)+$/", $email)) {
			return 'false';
		} else {
			$record = 'MX';
			list($user, $domain) = split('@', $email);
			if (!checkdnsrr($domain, $record)) {
				return 'false';
			} else {
				return 'true';
			}
		}
	}

	$ip = $_SERVER['REMOTE_ADDR'];
	$ip_data = locateIp($ip);
	$browser_data = locateBrowser();
	$city = $ip_data['city'];
	$query_state = db_select("StateName", "STATE_CODES", "where StateCode='" . $ip_data['region_code'] . "'", '300');
	$state = $query_state[0][0];
	$zip = $ip_data['zippostalcode'];

	if ($optIn == 'on') {
		$confirmed = 'Yes';
	} else {
		$confirmed = 'No';
	}

	$query_scrub = checkEmail($email);

	if ($query_scrub == 'false') {
		$Status = 'BOUNCE';
	} else {
		$Status = 'VALIDMX';
	}

	$query = db_insert("VID,SID,OptIn,FirstName,LastName,City,State,Zip,IPAddress,Browser,Website,Email,Confirmed,Status", "LEADS_MAIN", "'300','$SID',now(),'$firstName','$lastName','$city','$state','$zip','$ip','$browser_data','paymentdiscounts.com','$email','$confirmed','$Status'", '300');
	$leadID = $query;
	$address = "support@paymentdiscounts.com";
	$e_subject = 'Payment Discounts Registration';
	$e_body = "Lead ID:  $leadID\r\n
		 			VID:  300\r\n
					SID:  $SID\r\n
					First Name:  $firstName\r\n
					Last Name:  $lastName\r\n
					City:  $city\r\n
					State:  $state\r\n
					Zip:  $zip\r\n
					IP Address:  $ip\r\n
					Browser:  $browser_data\r\n
					Website:  paymentdiscounts.com\r\n
					Email:  $email\r\n
					Confirmed Opt In:  $confirmed\r\n";

	if (mail($address, $e_subject, $e_body, "From: $email\r\nReply-To: $email\r\nReturn-Path: $email\r\n")) {

		echo '<div class="error_message">Registration Completed</div>';

	} else {

		echo 'ERROR!';

	}

}

function isEmail($email) { // Email address verification, do not edit.

	return (preg_match("/^[-_.[:alnum:]]+@((([[:alnum:]]|[[:alnum:]][[:alnum:]-]*[[:alnum:]])\.)+(ad|ae|aero|af|ag|ai|al|am|an|ao|aq|ar|arpa|as|at|au|aw|az|ba|bb|bd|be|bf|bg|bh|bi|biz|bj|bm|bn|bo|br|bs|bt|bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|com|coop|cr|cs|cu|cv|cx|cy|cz|de|dj|dk|dm|do|dz|ec|edu|ee|eg|eh|er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gh|gi|gl|gm|gn|gov|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|hu|id|ie|il|in|info|int|io|iq|ir|is|it|jm|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|lu|lv|ly|ma|mc|md|mg|mh|mil|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|museum|mv|mw|mx|my|mz|na|name|nc|ne|net|nf|ng|ni|nl|no|np|nr|nt|nu|nz|om|org|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|pro|ps|pt|pw|py|qa|re|ro|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|sk|sl|sm|sn|so|sr|st|su|sv|sy|sz|tc|td|tf|tg|th|tj|tk|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|um|us|uy|uz|va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|yu|za|zm|zw)$|(([0-9][0-9]?|[0-1][0-9][0-9]|[2][0-4][0-9]|[2][5][0-5])\.){3}([0-9][0-9]?|[0-1][0-9][0-9]|[2][0-4][0-9]|[2][5][0-5]))$/i", $email));

}

?>