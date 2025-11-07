<?php
$mysqli = new mysqli("localhost", "root", "", "ecv");

if ($mysqli->connect_error) {
    die("Connection failed: " . $mysqli->connect_error);
}

// Build assignment lookup
$assignments = [];
$res = $mysqli->query("SELECT member_assignment_id, member_assignment FROM member_assignments");
while ($row = $res->fetch_assoc()) {
    $assignments[strtolower(trim($row['member_assignment']))] = $row['member_assignment_id'];
}

// Prepare insert statement
$insert_stmt = $mysqli->prepare("INSERT INTO members_x_member_assignments (member_id, member_assignment_id) VALUES (?, ?)");

if (!$insert_stmt) {
    die("Prepare failed: " . $mysqli->error);
}

// Get all members
$res = $mysqli->query("SELECT member_id, assignment_1, assignment_2, assignment_3 FROM members");

while ($row = $res->fetch_assoc()) {
    $member_id = $row['member_id'];
    $inserted = false;

    foreach (['assignment_1', 'assignment_2', 'assignment_3'] as $field) {
        $assignment = strtolower(trim($row[$field]));
        if (!empty($assignment) && isset($assignments[$assignment])) {
            $assignment_id = $assignments[$assignment];
            $insert_stmt->bind_param("ii", $member_id, $assignment_id);
            $insert_stmt->execute();
            $inserted = true;
        }
    }

    // If no assignments found, insert 'None'
    if (!$inserted) {
        $assignment_id = 1; // 'None'
        $insert_stmt->bind_param("ii", $member_id, $assignment_id);
        $insert_stmt->execute();
    }
}

$insert_stmt->close();
$mysqli->close();

echo "Finished updating members_x_member_assignments.\n";
?>

