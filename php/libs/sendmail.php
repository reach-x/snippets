#!/usr/bin/php
<?php
    define('LOGFILE','/tmp/fakesendmail.log');

    $log = fopen (LOGFILE,'a+');

    fwrite($log, "\n" . implode(' ',$argv) . " called on : " . date('Y-m-d H:i:s') . "\n");
    fwrite($log, file_get_contents("php://stdin"));
    fwrite($log, "\n===========================================================\n");
    fclose($log);

