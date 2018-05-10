#!/usr/bin/php

<?php
date_default_timezone_set("US/Pacific");

class source_database {

    public $username = "root";
    public $password = "<password>";
    public $name = "lbc";
    public $server = "<ip_address>";
    public $port = 3306;
}

class destination_database {

    public $username = "root";
    public $password = "<password>";
    public $name = "lbc";
    public $server = "<ip_address>";
    public $port = 3306;
}

$source_database = new source_database();
$destination_database = new destination_database();


$owner_post_id = 40445553;
$last_owner_post_id = 478608449;;
while ($owner_post_id <= $last_owner_post_id) {

    $source_mysqli = new mysqli( $source_database->server, $source_database->username, $source_database->password, $source_database->name, $source_database->port );
    $destination_mysqli = new mysqli( $destination_database->server, $destination_database->username, $destination_database->password, $destination_database->name, $destination_database->port );

    if ($source_mysqli->connect_errno || $destination_mysqli->connect_errno) {
        printf("Error at owner_post_id: %d source:%s %s destination:%s %s\n", $owner_post_id, $source_mysqli->connect_errno, $source_mysqli->connect_error, $destination_mysqli->connect_errno, $destination_mysqli->connect_error);
    }

    $sql = sprintf("SELECT * FROM owner_posts_data_restore WHERE owner_post_id=%d", $owner_post_id);
    $result = $source_mysqli->query($sql);
    $source_data = array();

    while ($row = $result->fetch_array(MYSQLI_ASSOC)) {
        $destination_statement = $destination_mysqli->prepare("REPLACE INTO owner_posts_data_restore (owner_post_id, field_id, content) VALUES (?,?,?)");
        $destination_statement->bind_param("iis", $row['owner_post_id'], $row['field_id'], $row['content']);
        $destination_statement->execute();
        $destination_statement->close();
    }

    if ($owner_post_id % 1000 == 0) {
        printf("%d\n", $owner_post_id);
    }

    $owner_post_id++;

}
