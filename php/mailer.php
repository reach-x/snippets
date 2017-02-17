<?php
$errno = '';
$errstr = '';
$timeout = 5;
$streamContext = stream_context_create();
$host = 'ssl://smtp.gmail.com:465';
$stream = stream_socket_client($host, $errno, $errstr, $timeout, STREAM_CLIENT_CONNECT, $streamContext);
stream_set_blocking($stream, 1);
stream_set_timeout($stream, $timeout);
stream_set_write_buffer($stream, 0);

$fgets = fgets($stream);
print_r($fgets);

fwrite($stream, "ehlo [127.0.0.1]\r\n");

$response = '';
do {
    $line = fgets($stream);
    $response .= $line;
} while (null !== $line && false !== $line && ' ' != $line{3});

print_r($response);

fclose($stream);

