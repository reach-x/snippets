<?php
$zipFilename = 'number_files.zip';
$zip = new ZipArchive();
$zip->open($zipFilename, ZipArchive::CREATE);

for ($i = 1; $i <= 100; $i++) {
	$number = str_pad($i, 3, '0', STR_PAD_LEFT); // Pad numbers with leading zeros if necessary
	$filename = $number . '.mp3';
	$content = generateAudioFileContent($i); // Replace this with your own audio file generation logic

	// Add the file to the zip archive
	$zip->addFromString($filename, $content);
}

$zip->close();

// Force the download of the zip file
header('Content-Type: application/zip');
header('Content-Disposition: attachment; filename="' . $zipFilename . '"');
header('Content-Length: ' . filesize($zipFilename));
readfile($zipFilename);

// Delete the zip file from the server
unlink($zipFilename);

function generateAudioFileContent($number)
{
	// Replace this with your own audio file generation logic
	$textToSpeech = new TextToSpeech(); // Assuming you have a TextToSpeech class or library

	// Generate the audio content using the number
	$text = 'Number ' . $number;
	$audioContent = $textToSpeech->generateAudio($text); // Generate audio from the text

	return $audioContent;
}

class TextToSpeech
{
	private $apiKey;
	private $client;

	public function __construct($apiKey)
	{
		$this->apiKey = $apiKey;
		$this->client = new GuzzleHttp\Client();
	}

	public function generateAudio($text)
	{
		$url = 'https://texttospeech.googleapis.com/v1/text:synthesize?key=' . $this->apiKey;

		$payload = [
			'input'       => [
				'text' => $text,
			],
			'voice'       => [
				'languageCode' => 'en-US',
				'ssmlGender'   => 'FEMALE',
			],
			'audioConfig' => [
				'audioEncoding' => 'MP3',
			],
		];

		$response = $this->client->post($url, [
			'headers' => [
				'Content-Type' => 'application/json',
			],
			'body'    => json_encode($payload),
		]);

		$audioContent = $response->getBody()->getContents();
		return $audioContent;
	}
}
