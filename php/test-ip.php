<?php


$ip="1.2.3.4";
$ip=null;
$long="16909060";

//printf("%s ->  %s\n",$ip,ip2long($ip));


if (is_numeric($ip)){
	printf("ip: %s ->  %s\n",$ip,ip2long($ip));
} else {
	printf("ip: not numeric\n");
}


if (is_numeric($long)){
	printf("long: %s ->  %s\n",$ip,ip2long($ip));
} else {
	printf("long: not numeric\n");
}



