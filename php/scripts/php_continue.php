<?php

$routing_rule_filters = array(
	array(
		'routing_rule_filter_type_id' => 1,
		'field_name' => "state",
		'criteria' => "CA",
	),
	array(
		'routing_rule_filter_type_id' => 2,
	  'field_name' => "age",
	  'criteria' => 18,
	),
);

$owner_posts = array(
	array(
		'person_id' => 1,
		'state' => 'CA',
		'age' => 21,
	),
	array(
		'person_id' => 2,
		'state' => 'CA',
		'age' => 13,
	),
	array(
		'person_id' => 3,
		'state' => 'TX',
		'age' => 21,
	),
	array(
		'person_id' => 4,
	  'state' => 'TX',
	  'age' => 13,
	),
);

$is_filtered = TRUE;

foreach ($owner_posts as $owner_post) {
	foreach ($routing_rule_filters as $routing_rule_filter) {
		$person = $owner_post['person_id'];
		$field_name = $routing_rule_filter['field_name'];
		$value = $owner_post[$field_name];
		$criteria = $routing_rule_filter['criteria'];

	  printf("%s: filtering %s / %s criteria %s: ",$person,$field_name,$value,$criteria);

		if (isset( $owner_post[$field_name] )) {
			switch ($routing_rule_filter['routing_rule_filter_type_id']) {
				case 1:
				  // filter out if value not equal to $criteria
	    		$is_filtered = ( strtolower($value) != strtolower($criteria) );

					break;

				case 2:
				  // filter out if value is less than criteria
					$is_filtered = ( intval($value) < intval($criteria) );
					break;

				default:
				       print "default case\n";
				       break;
			}
		  print ($is_filtered ? "true" : "false") . "\n";

			if($is_filtered){
				print "\n";
				continue 2;
			}
		}
	}
}
