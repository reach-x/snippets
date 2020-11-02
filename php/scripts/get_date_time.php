<?php


print_r(get_start_end_dates(FALSE,FALSE));

function get_start_end_dates($start_date, $end_date) {

		if ($start_date === FALSE) {
			$start_date_timestamp = strtotime("2 days ago");
		} elseif (is_numeric($start_date)) {
			$start_date_timestamp = $start_date;
		} else {
			$start_date_timestamp = strtotime("{$start_date}");
		}

		if ($start_date_timestamp == FALSE) {
			$start_date = "error converting start date";
		} else {
			//$start_date = date("Y-m-d H:i:s", strtotime("{$start_date}") - 7200);
			$start_date = date("Y-m-d H:i:s", $start_date_timestamp);
		}

		if ($end_date === FALSE) {
			$end_date_timestamp = strtotime("now");
		} elseif (is_numeric($end_date)) {
			$end_date_timestamp = $end_date;
		} else {
			$end_date_timestamp = strtotime("{$end_date}");
		}

		if ($end_date_timestamp == FALSE) {
			$end_date = "error converting end date";
		} else {
			$end_date = date("Y-m-d H:i:s", $end_date_timestamp);
		}

		return array(
			$start_date,
			$end_date,
		);
	}
