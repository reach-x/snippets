<?php

$owner_post = array();
$owner_post['GENERATED_AT'] = "2021-04-22 12:36:33";

       if (isset($owner_post['GENERATED_AT']) && strtotime($owner_post['GENERATED_AT'])) {

            $generated_at_time = intval(date("His", strtotime($owner_post['GENERATED_AT'])));

            if ($generated_at_time == 0) {
                $current_time = date("H:i:s", strtotime($owner_post['received_at']));
                $owner_post['GENERATED_AT'] = sprintf("%s %s", date("Y-m-d", strtotime($owner_post['GENERATED_AT'])), $current_time);
            }

            $generated_at_date = date("Y-m-d H:i:s", strtotime($owner_post['GENERATED_AT']));
        } else {
            $generated_at_date = date("Y-m-d H:i:s");
        }



print_r(
	array(
		'owner_post'=>$owner_post,
		$generated_at_date = date("Y-m-d H:i:s", strtotime($owner_post['GENERATED_AT']))

	)
);
