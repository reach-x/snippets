<?php


$message = "<p>Hello “something in quotes” goodby</p>";
echo("pre message: $message\n");
$new_message = iconv("UTF-8", "UTF-8//TRANSLIT",$message);
echo("new message: $new_message\n");
$doc = new DOMDocument();
$doc->loadHTML(utf8_decode($message));
$body = $doc->getElementsByTagName('body')->item(0);
$message=$doc->saveHTML($body);
$new_message = iconv("UTF-8", "UTF-8//TRANSLIT",$message);
echo("new message: $new_message\n");
echo("Modified message: $message\n");


