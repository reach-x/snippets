<?php

function processCsvToSql($inputFile, $outputFile)
{
	// Arrays to store unique entries
	$years = [];
	$makes = [];
	$models = [];
	$bodyStyles = [];
	$cars = [];

	// Open the input CSV file
	if (($infile = fopen($inputFile, 'r')) !== FALSE) {
		// Read each line from the CSV file
		while (($row = fgetcsv($infile)) !== FALSE) {
			// Expecting format: year, make, model, body styles
			list($year, $make, $model, $bodyStylesStr) = $row;

			// Add unique entries for years, makes, models, and body_styles
			if (!isset($years[$year])) {
				$years[$year] = "INSERT INTO years (year) VALUES ($year) ON DUPLICATE KEY UPDATE year_id=year_id;";
			}
			if (!isset($makes[$make])) {
				$makes[$make] = "INSERT INTO makes (make_name) VALUES ('$make') ON DUPLICATE KEY UPDATE make_id=make_id;";
			}
			if (!isset($models[$model])) {
				$models[$model] = "INSERT INTO models (model_name) VALUES ('$model') ON DUPLICATE KEY UPDATE model_id=model_id;";
			}

			// Split body styles if there are multiple
			$bodyStylesList = array_map('trim', explode(',', str_replace('"', '', $bodyStylesStr)));
			foreach ($bodyStylesList as $style) {
				if (!isset($bodyStyles[$style])) {
					$bodyStyles[$style] = "INSERT INTO body_styles (body_style_name) VALUES ('$style') ON DUPLICATE KEY UPDATE body_style_id=body_style_id;";
				}

				// Create a unique key for cars to avoid duplicates
				$carKey = "$year|$make|$model|$style";
				if (!isset($cars[$carKey])) {
					$cars[$carKey] = "INSERT INTO cars (year_id, make_id, model_id, body_style_id) VALUES (
                        (SELECT year_id FROM years WHERE year = $year),
                        (SELECT make_id FROM makes WHERE make_name = '$make'),
                        (SELECT model_id FROM models WHERE model_name = '$model'),
                        (SELECT body_style_id FROM body_styles WHERE body_style_name = '$style')
                    );";
				}
			}
		}
		fclose($infile);
	} else {
		die("Error: Unable to open input file.\n");
	}

	// Write all unique SQL statements to the output file
	if (($outfile = fopen($outputFile, 'w')) !== FALSE) {
		fwrite($outfile, "-- Insert statements for years\n");
		fwrite($outfile, implode("\n", $years) . "\n\n");

		fwrite($outfile, "-- Insert statements for makes\n");
		fwrite($outfile, implode("\n", $makes) . "\n\n");

		fwrite($outfile, "-- Insert statements for models\n");
		fwrite($outfile, implode("\n", $models) . "\n\n");

		fwrite($outfile, "-- Insert statements for body_styles\n");
		fwrite($outfile, implode("\n", $bodyStyles) . "\n\n");

		fwrite($outfile, "-- Insert statements for cars\n");
		fwrite($outfile, implode("\n", $cars) . "\n");

		fclose($outfile);
		echo "SQL file has been generated successfully: $outputFile\n";
	} else {
		die("Error: Unable to open output file for writing.\n");
	}
}

// Specify the input and output files
$inputFile = '/Users/jbrahy/Projects/app.webfunctions.net/data/us-car-models-data/processed_all_years.csv';  // Replace with the actual CSV file path
$outputFile = '/Users/jbrahy/Projects/app.webfunctions.net/data/us-car-models-data/output.sql';

// Run the function to process CSV and generate SQL
processCsvToSql($inputFile, $outputFile);
