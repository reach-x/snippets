<?php

$name = <<<EOS
Include the script you wrote to crack this encryption. 
Make it pretty, ulgy code doesn't get an interview.
Send it to:

John Brahy, CTO
tech-jobs@popularllc.com
Use "Cracked for Interview" as the email subject
EOS;


$encrypted_text = array();

foreach (str_split($name) as $letter) {
	printf("%s,\n", $letter);
	$encrypted_text[] = ord($letter);
}

printf("%s\n", json_encode($encrypted_text));
