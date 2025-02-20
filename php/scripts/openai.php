<?php

function sendPromptToOpenAI($prompt, $apiKey) {
    $url = "https://api.openai.com/v1/chat/completions";
    
    // Correctly structured data for the chat completion
    $data = [
        "model" => "gpt-4",  // Use "gpt-3.5-turbo" if you prefer GPT-3.5
        "messages" => [
            ["role" => "user", "content" => $prompt]
        ],
        "temperature" => 0.7
    ];

    // Initialize cURL
    $ch = curl_init($url);
    
    // Set cURL options
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($ch, CURLOPT_HTTPHEADER, [
        "Content-Type: application/json",
        "Authorization: Bearer $apiKey"
    ]);
    curl_setopt($ch, CURLOPT_POST, true);
    curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($data));

    // Execute the cURL request and capture the response
    $response = curl_exec($ch);

    // Check for errors
    if (curl_errno($ch)) {
        echo 'Error:' . curl_error($ch);
    }

    // Close cURL session
    curl_close($ch);

    // Decode the JSON response
    $result = json_decode($response, true);

    // Return the generated response from OpenAI
    return $result['choices'][0]['message']['content'] ?? "No response received";
}

// Usage
$apiKey = "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";

$prompt = "What is the capital of France?";
$response = sendPromptToOpenAI($prompt, $apiKey);

echo "Response from OpenAI: " . $response;

