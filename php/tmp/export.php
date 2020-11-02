<?php

$sql = <<<EOQ
SELECT unsubscribe_id, phone_number, email_address, ip_address, user_agent, referer, transaction_id, affiliate_id, created_at  FROM unsubscribes WHERE exported=0;
EOQ;

$database = new database();
$mysqli = $database->connect();
$statement = $mysqli->prepare( $sql );
$statement->execute();
$result = $statement->get_result();

$filename = sprintf( "export-%s.csv", date( "Ymdhis" ) );
$filepath = sprintf( "/var/www/html/web-system-sites/join-class-action.com/exports/%s", $filename );
$file_handle = fopen( $filepath, "w" );

fprintf( $file_handle, "unsubscribe_id, phone_number, email_address, ip_address, user_agent, referer, created_at\n" );
$unsubscribe_ids = array();

while ( $row = $result->fetch_assoc() ) {
    $unsubscribe_ids[] = $row['unsubscribe_id'];
    fputcsv( $file_handle, $row );
}
fclose( $file_handle );

$statement->close();
$statement = $mysqli->prepare( "UPDATE unsubscribes SET exported=1 WHERE unsubscribe_id=?" );

foreach ( $unsubscribe_ids as $unsubscribe_id ) {
    $statement->bind_param( "d", $unsubscribe_id );
    $statement->execute();
}

$statement->close();
$mysqli->close();

$headers = "From: Join Class Action Unsubscribe Request Form <noreply@popularllc.com>\r\n" . "X-Mailer: PHP/" . phpversion() . "\r\n";
$message = "\n\nExport ready for download\n\https://join-class-action.com/exports/{$filename}";

mail( "jb@popularllc.com", "JCA Download Ready", $message, $headers );

class database {

    public $database = "<database_name>";
    public $port = 3306;
    public $server = '127.0.0.1';
    public $username = 'unsub';
    public $password = '<password>';

    function connect() {

        $mysqli = new mysqli( $this->server, $this->username, $this->password, $this->database, $this->port );

        if ( $mysqli->connect_error ) {
            print_r( array(
                'error'         => $mysqli->error,
                'connect_error' => $mysqli->connect_error,
                'opts'          => array(
                    $this->server,
                    $this->username,
                    $this->password,
                    $this->database,
                    $this->port,
                ),
            ) );
            die( 'Connect Error (' . $mysqli->connect_errno . ') ' . $mysqli->connect_error );
        }

        return $mysqli;
    }
}
