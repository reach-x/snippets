<?php

// Database connection
$servername = "localhost";
$username = "username";
$password = "password";
$dbname = "database_name";

// Create connection
$conn = new mysqli($servername, $username, $password, $dbname);

// Check connection
if ($conn->connect_error) {
	die("Connection failed: " . $conn->connect_error);
}

// OpenAI API url
$url = 'https://api.openai.com/v1/engines/davinci-codex/completions';

// Initialize cURL
$ch = curl_init($url);

// API key
$api_key = "your-openai-api-key";

// Your prompt
$prompt = "Translate the following English text to French: '{'text': 'Hello, world!'}'";

// Create the payload
$data = array(
	'prompt'     => $prompt,
	'max_tokens' => 60,
);

// Prepare new cURL resource
curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
curl_setopt($ch, CURLINFO_HEADER_OUT, TRUE);
curl_setopt($ch, CURLOPT_POST, TRUE);
curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($data));

// Set HTTP Header for POST request
curl_setopt($ch, CURLOPT_HTTPHEADER, array(
	'Content-Type: application/json',
	'Authorization: Bearer ' . $api_key,
));

// Submit the POST request
$result = curl_exec($ch);

// Check if any error occurred
if (curl_errno($ch)) {
	echo 'Request Error:' . curl_error($ch);
}

// Close cURL session handle
curl_close($ch);

// Decode the response
$response = json_decode($result, TRUE);

// Retrieve the 'choices' array which contains the response from the model
$choices = $response['choices'];

// If choices exist
if (!empty($choices)) {
	// Prepare an SQL statement to insert the response into the database
	$stmt = $conn->prepare("INSERT INTO responses (response) VALUES (?)");

	// For each response
	foreach ($choices as $choice) {
		// Bind the response to the SQL statement
		$stmt->bind_param("s", $choice['text']);

		// Execute the SQL statement
		$stmt->execute();
	}

	// Close the statement
	$stmt->close();
}

// Close the connection
$conn->close();
?>
