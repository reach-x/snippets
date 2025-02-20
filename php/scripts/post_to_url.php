<?php
// Path to the external JSON file
$jsonFilePath = '/Users/jbrahy/Projects/snippets/tmp/sample.json';

// Read the JSON file contents
$jsonData = file_get_contents($jsonFilePath);

// Check if the file was read successfully
if ($jsonData === false) {
	die('Error reading the JSON file.');
}

// Decode the JSON data to ensure it's valid
$data = json_decode($jsonData, true);
if (json_last_error() !== JSON_ERROR_NONE) {
	die('Invalid JSON data.');
}

// Initialize cURL session
$ch = curl_init();

// Set the URL to which the request will be sent
$url = 'https://popularllc.leadspediatrack.com/pre-ping.do';

// Set cURL options
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_POST, true);
curl_setopt($ch, CURLOPT_POSTFIELDS, $jsonData);
curl_setopt($ch, CURLOPT_HTTPHEADER, [
	'Content-Type: application/json',
	'Content-Length: ' . strlen($jsonData)
]);

// Execute the cURL session
$response = curl_exec($ch);

// Check for errors
if (curl_errno($ch)) {
	echo 'cURL error: ' . curl_error($ch);
} else {
	// Process the response as needed
	echo 'Response: ' . $response;
}

// Close the cURL session
curl_close($ch);
