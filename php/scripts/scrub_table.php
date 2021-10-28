<?php
/*

    Project: LBC
    Built by: jbrahy
    Filename: get_primary_ids.php
    Started At: 3/26/20 13:03
    Using: PhpStorm
*/

$database = new \database();
$primary_keys_at_zero = array();
$database->server = "database.pm.internal";
$mysqli = $database->connect();
$sql = "SELECT email_address_id, email_address FROM temp.sight_savers_email_and_name";
$result = $mysqli->query($sql);
$records = array();

while ($row = $result->fetch_array(MYSQLI_ASSOC)) {

    $records["{$row['email_address_id']}"] = $row['email_address'];
}

printf("Completed database pull with %d records\n", count($records));
$counter = 0;

foreach ($records as $email_address_id => $email_address) {

    $url = "https://api.emailoversight.com/api/emailvalidation?apitoken=c37f9d2d-fccd-4308-9e15-e643eaf32233&listid=3799&email={$email_address}";
    $http_response = file_get_contents($url);
    $response = json_decode($http_response, TRUE);

    if (json_last_error()) {
        printf("\nJSON PARSE ERROR: (%d) %s -> %s\n", $email_address_id, $email_address, $http_response);
    } else {
        $sql = <<<EOQ
INSERT INTO sight_savers_scrub_log 
    (email_address_id,response_code,response_text,email_domain_group_id,email_domain_group) 
VALUES
    ($email_address_id,{$response['ResultId']},"{$response['Result']}",{$response['EmailDomainGroupId']},"{$response['EmailDomainGroup']}")
EOQ;
        $mysqli->query($sql);

        $sql = <<<EOQ
UPDATE temp.sight_savers_email_and_name 
    SET scrub_status_id={$response['ResultId']}, 
        email_domain_group_id={$response['EmailDomainGroupId']}, 
        email_domain_group='{$response['EmailDomainGroup']}' 
WHERE email_address_id={$email_address_id}
EOQ;

        $mysqli->query($sql);
    }

    if ($counter % 50 == 0) {
        printf("%d\n", $response['ResultId']);
    } else {
        printf("%d:", $response['ResultId']);
    }
    $counter++;
}

class database
{

    public $username = "root";
    public $password = "<password>";
    public $database = "temp";
    public $server = "database.pm.internal";
    public $port = 3306;

    function connect()
    {

        $mysqli = new \mysqli($this->server, $this->username, $this->password, $this->database, $this->port);

        if ($mysqli->connect_error) {
            die('Connect Error (' . $mysqli->connect_errno . ') ' . $mysqli->connect_error);
        }

        return $mysqli;
    }
}
