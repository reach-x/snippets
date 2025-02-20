<?php

function generateSimpleSqlInserts($inputFile, $outputFile) {
	// Arrays to store unique entries with assigned IDs
	$years = [];
	$makes = [];
	$models = [];
	$bodyStyles = [];
	$cars = [];

	// Initialize ID counters for each table
	$yearId = 1;
	$makeId = 1;
	$modelId = 1;
	$bodyStyleId = 1;
	$carId = 1;

	// Open the input CSV file
	if (($infile = fopen($inputFile, 'r')) !== false) {
		// Read each line from the CSV file
		while (($row = fgetcsv($infile)) !== false) {
			list($year, $make, $model, $bodyStylesStr) = $row;

			// Assign unique IDs to years
			if (!isset($years[$year])) {
				$years[$year] = $yearId++;
			}

			// Assign unique IDs to makes
			if (!isset($makes[$make])) {
				$makes[$make] = $makeId++;
			}

			// Assign unique IDs to models
			if (!isset($models[$model])) {
				$models[$model] = $modelId++;
			}

			// Split body styles and assign unique IDs
			$bodyStylesList = array_map('trim', explode(',', str_replace('"', '', $bodyStylesStr)));
			foreach ($bodyStylesList as $style) {
				if (!isset($bodyStyles[$style])) {
					$bodyStyles[$style] = $bodyStyleId++;
				}

				// Add a car entry for each body style
				$cars[] = [
					'year_id' => $years[$year],
					'make_id' => $makes[$make],
					'model_id' => $models[$model],
					'body_style_id' => $bodyStyles[$style]
				];
			}
		}
		fclose($infile);
	} else {
		die("Error: Unable to open input file.\n");
	}

	// Open the output file for writing SQL
	if (($outfile = fopen($outputFile, 'w')) !== false) {
		// Generate SQL inserts for each table
		fwrite($outfile, "-- Insert statements for years\n");
		foreach ($years as $year => $id) {
			fwrite($outfile, "INSERT INTO years (year_id, year) VALUES ($id, $year);\n");
		}

		fwrite($outfile, "\n-- Insert statements for makes\n");
		foreach ($makes as $make => $id) {
			fwrite($outfile, "INSERT INTO makes (make_id, make_name) VALUES ($id, '$make');\n");
		}

		fwrite($outfile, "\n-- Insert statements for models\n");
		foreach ($models as $model => $id) {
			fwrite($outfile, "INSERT INTO models (model_id, model_name) VALUES ($id, '$model') ON DUPLICATE KEY UPDATE model_id=model_id;\n");
		}

		fwrite($outfile, "\n-- Insert statements for body_styles\n");
		foreach ($bodyStyles as $style => $id) {
			fwrite($outfile, "INSERT INTO body_styles (body_style_id, body_style_name) VALUES ($id, '$style') ON DUPLICATE KEY UPDATE body_style_id=body_style_id;\n");
		}

		fwrite($outfile, "\n-- Insert statements for cars\n");
		foreach ($cars as $car) {
			fwrite($outfile, "INSERT INTO cars (year_id, make_id, model_id, body_style_id) VALUES ({$car['year_id']}, {$car['make_id']}, {$car['model_id']}, {$car['body_style_id']});\n");
		}

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
generateSimpleSqlInserts($inputFile, $outputFile);

