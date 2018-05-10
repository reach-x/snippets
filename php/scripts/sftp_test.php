<?php


$hostname = "<hostname>";
$username = "<username>";
$password = "<password>";

$destination_file = 'jb_test.txt';
$source_file = '/tmp/test.txt';

$sftp_url = sprintf('ssh2.sftp://%s:%s@%s:22/uploads/%s', $username, $password, $hostname, $destination_file);

if (file_put_contents($sftp_url, file_get_contents($source_file))) {
    printf("success\n");
} else {
    printf("failure\n");
}
