<?php
// Open a file to store the results
$file = fopen("nanp_phone_numbers_md5.csv", "w");

// Loop through valid area codes (NXX)
for ($area = 200; $area <= 999; $area++) {
    if ($area % 100 == 11) continue; // Skip reserved codes like 911, 211, etc.
	echo $area;

    // Loop through valid exchange codes (NXX)
    for ($exchange = 200; $exchange <= 999; $exchange++) {
        if ($exchange % 100 == 11) continue; // Skip reserved exchanges

        // Loop through all line numbers (XXXX)
        for ($line = 0; $line <= 9999; $line++) {
            $phone_number = sprintf("%03d%03d%04d", $area, $exchange, $line);
            $md5_hash = md5($phone_number);
            fputcsv($file, [$phone_number, $md5_hash], ',', '"', ''); // Write to CSV
        }
    }
}

// Close the file
fclose($file);
echo "Finished generating NANP phone numbers and MD5 hashes.\n";

