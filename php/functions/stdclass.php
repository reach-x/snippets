<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 4/18/18
 * Time: 11:07
 */
$username = "username";
$password = "password";
$list_id = "list_id";
$email_addresss = array(
	"asdf@asdf.com",
	"asdf@asdf.com",
	"asdf@asdf.com",
	"asdf@asdf.com",
	"asdf@asdf.com",
	"asdf@asdf.com",
);
$topica_command = new stdClass();
$topica_command->topicaAction->username = $username;
$topica_command->topicaAction->password = $password;
$topica_command->topicaAction->subscriberAdd->list = $list_id;
$topica_command->topicaAction->subscriberAdd->data->source = "inline";
$topica_command->topicaAction->subscriberAdd->data->content = $email_addresss;

print json_encode($topica_command);
