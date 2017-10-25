<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 9/8/17
 * Time: 17:39
 */

$data = array(
	'keyword_id' => 1102935,
	'keyword_name' => 'PPMAMZ1',
	'affiliate_id' => 270579,
	'hit_id' => 687326571,
	'leads' => 0,
	'revenue' => 0.00,
);


if (is_float($data['revenue']) ) {
	printf("revenue passes\n");
} else {
	printf("revenue doesn't pass\n");
}


if (is_numeric($data['keyword_id']) ) {
	printf("keyword_id passes\n");
} else {
	printf("keyword_id doesn't pass\n");
}


if (is_numeric($data['leads'])) {
	printf("leads passes\n");
} else {
	printf("leads doesn't pass\n");
}


if (is_numeric($data['hit_id'])) {
	printf("hit_id passes\n");
} else {
	printf("hit_id doesn't pass\n");
}


