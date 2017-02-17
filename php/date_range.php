<?php

$params = array(
			'start_date' => ($start_date == "" ? date("Y-m-d", strtotime("today -30 days")) : $start_date),
			'end_date' => ($end_date == "" ? date("Y-m-d", strtotime("yesterday")) : $end_date),
			'debug' => $debug,
		);

print_r(array('create_date_range_array'=>create_date_range_array($params['start_date'], $params['end_date'])));




	function create_date_range_array($date_from, $date_to) {

		$date_range = array();
		$date_from_unixtime = mktime(1, 0, 0, substr($date_from, 5, 2), substr($date_from, 8, 2), substr($date_from, 0, 4));
		$date_to_unixtime = mktime(1, 0, 0, substr($date_to, 5, 2), substr($date_to, 8, 2), substr($date_to, 0, 4));

		print_r(array('date_to_unixtime'=>$date_to_unixtime));


		if ($date_to_unixtime >= $date_from_unixtime) {
			array_push($date_range, date('Y-m-d', $date_from_unixtime)); // first entry

			while ($date_from_unixtime < $date_to_unixtime) {
				$date_from_unixtime += 86400; // add 24 hours
				array_push($date_range, date('Y-m-d', $date_from_unixtime));
			}
		}

		return $date_range;
	}

