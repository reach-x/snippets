<?php

// open IMAP connection
$mail = imap_open('{imap.gmail.com:993/imap/ssl}INBOX','seeds@reach-x.com', 'Ve7FnF*W');

// or, open POP3 connection
//$mail = imap_open('{:110/pop3}', 'username', 'password');

// grab a list of all the mail headers
$headers = imap_headers($mail);
printf("%s\n\n",print_r($headers,TRUE));

// grab a header object for the last message in the mailbox
$last = imap_num_msg($mail);
printf("%s\n\n",print_r($last,TRUE));

$header = imap_header($mail, $last);
printf("%s\n\n",print_r($header,TRUE));

// grab the body for the same message
$body = imap_body($mail, $last);

printf("%s\n\n",print_r($body,TRUE));

// close the connection
imap_close($mail);
