<?php
require_once __DIR__ . '/../libs/phpqrcode/qrlib.php';
error_reporting(E_ALL & ~E_DEPRECATED);

// URLs array
$urls = array(
	"Doin's Signup"     => "https://www.ecv-58.org/member-signup.html",
	"PBC Signup"        => "https://www.ecv-58.org/pbc-signup.html",
	"Change of Address" => "https://www.ecv-58.org/change-of-address.html",
	"Hawkers Corner"    => "https://www.ecv-58.org/hawkers-corner.html",
	"Ways to Help"      => "https://www.ecv-58.org/ways-to-help.html",
);

// File path for storing QR codes
$outputDir = __DIR__ . '/../tmp/';

// Create the output directory if it doesn't exist
if (!is_dir($outputDir)) {
	mkdir($outputDir, 0755, TRUE);
}

// Path to the logo image
$logoPath = __DIR__ . "/../images/ecv58-logo.png"; // Replace with the correct path to your logo

// Verify logo file exists
if (!file_exists($logoPath)) {
	die("Error: Logo file not found at $logoPath");
}

// Loop through each URL and generate QR codes
foreach ($urls as $name => $link) {
	// Generate the filename, removing ".html"
	$filename = $outputDir . 'ecv-58-' . strtolower(str_replace([' ', "'", "/", ".html", ":"], '-', parse_url($link, PHP_URL_PATH))) . ".png";

	// Generate the QR code
	QRcode::png($link, $filename, QR_ECLEVEL_H, 10);

	// Load the QR code and logo images
	$qrImage = imagecreatefrompng($filename);
	$logoImage = imagecreatefrompng($logoPath);

	// Get dimensions of both images
	$qrWidth = imagesx($qrImage);
	$qrHeight = imagesy($qrImage);
	$logoWidth = imagesx($logoImage);
	$logoHeight = imagesy($logoImage);

	// Calculate the logo size and position
	$logoSize = $qrWidth / 4; // Adjust logo size relative to QR code
	$logoX = ($qrWidth - $logoSize) / 2;
	$logoY = ($qrHeight - $logoSize) / 2;

	// Resize the logo and overlay it onto the QR code
	imagecopyresampled(
		$qrImage, $logoImage,
		$logoX, $logoY, 0, 0,
		$logoSize, $logoSize,
		$logoWidth, $logoHeight
	);

	// Save the final image
	imagepng($qrImage, $filename);

	// Clean up
	imagedestroy($qrImage);
	imagedestroy($logoImage);

	echo "QR code generated for: $name ($link) -> $filename<br>";
}