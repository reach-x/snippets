<?php

function processFile($inputFile, $outputFile)
{
	// Open the input file for reading
	if (($infile = fopen($inputFile, 'r')) !== FALSE) {
		// Open the output file for writing
		if (($outfile = fopen($outputFile, 'w')) !== FALSE) {
			// Process each line in the input file
			while (($data = fgetcsv($infile)) !== FALSE) {
				// Expect each row to have exactly 4 columns in the format: year, make, model, body_styles
				list($year, $make, $model, $bodyStyles) = $data;

				// Remove surrounding quotes and split body styles if there are multiple
				$bodyStylesArray = array_map('trim', explode(',', trim($bodyStyles, '"')));

				// Write each body style as a separate row in the output file
				foreach ($bodyStylesArray as $style) {
					fputcsv($outfile, [$year, $make, $model, $style]);
				}
			}
			fclose($outfile);
		} else {
			echo "Error: Unable to open output file for writing.\n";
		}
		fclose($infile);
	} else {
		echo "Error: Unable to open input file for reading.\n";
	}
}

// Specify the input and output file names
$inputFile = '/Users/jbrahy/Projects/app.webfunctions.net/data/us-car-models-data/all_years.csv';  // Replace with your actual input file name
$outputFile = '/Users/jbrahy/Projects/app.webfunctions.net/data/us-car-models-data/processed_all_years.csv';

// Process the file
processFile($inputFile, $outputFile);
echo "File processing complete. Check the output file: " . $outputFile . "\n";
