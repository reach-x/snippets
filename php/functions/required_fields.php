<?php

#$required_fields = array("email_address","zip_code");
#print json_encode($required_fields);
#print "\n";

print_r(array('required_fields' => json_decode($argv[1])));


