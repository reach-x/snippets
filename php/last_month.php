<?php


print_r(get_admin_listing(1, 2015));

function get_admin_listing($month, $year) {

	$last_date = sprintf("%d/1/%d - 1 month", $month, $year);
	$last_month = date("m", strtotime($last_date));
	$last_year = date("Y", strtotime($last_date));

	return (array(
		"last_date" => $last_date,
		"last_month" => $last_month,
		"last_year" => $last_year,
	));
}
