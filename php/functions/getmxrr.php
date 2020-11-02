<?php

$isp_domain="gmail.com";

$mx_hosts = array();
$mx_weights = array();
getmxrr($isp_domain,$mx_hosts,$mx_weights);
print_r(array(
	'mx_hosts'=>$mx_hosts,
	'mx_weights'=>$mx_weights,
));
