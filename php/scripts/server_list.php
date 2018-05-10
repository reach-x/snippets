<ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> <ip_address> 
<?php
exit;

//$mysqli = new mysqli('db-master.mypm.us', 'root', '<password>', 'lbc', 3306);
//$mysqli = new mysqli('db-slave.mypm.us', 'root', '<password>', 'lbc', 3306);
$mysqli = new mysqli('<ip_address>', 'root', '<password>', 'lbc', 3306);

if ( $mysqli->connect_error ) {
    die('Connect Error (' . $mysqli->connect_errno . ') ' . $mysqli->connect_error);
}
//$sql = "SELECT concat(hostname,'.',domain) as server_name FROM servers WHERE status_id=1 ORDER BY rand()";
$sql = "SELECT ip_address FROM servers WHERE status_id=1 ORDER BY server_id";

$servers = array();

/* If we have to retrieve large amount of data we use MYSQLI_USE_RESULT */
if ( $result = $mysqli->query($sql, MYSQLI_USE_RESULT) ) {

    while ( $row = $result->fetch_array() ) {
        $servers[] = $row['ip_address'];
    }

    if ( !$mysqli->query("SET @a:='this will not work'") ) {
        printf("Error: %s\n", $mysqli->error);
    }
    $result->close();
}

print join(" ", $servers);


