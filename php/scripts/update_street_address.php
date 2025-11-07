<?php
$pdo = new PDO("mysql:host=localhost;dbname=ecv", 'root', '');

$query = "SELECT member_id, street_address FROM members WHERE street_address IS NOT NULL AND street_address != ''";
$stmt = $pdo->query($query);
$members = $stmt->fetchAll(PDO::FETCH_ASSOC);

function titleCase($string) {
    // Lowercase the full string, then uppercase each word
    $string = strtolower($string);
    $string = preg_replace_callback('/\b\w+\b/', function ($matches) {
        return ucfirst($matches[0]);
    }, $string);
    return $string;
}

$update = $pdo->prepare("UPDATE members SET street_address = :address WHERE member_id = :id");

foreach ($members as $member) {
    $id = $member['member_id'];
    $original = $member['street_address'];
    $formatted = titleCase($original);

    if ($formatted !== $original) {
        $update->execute([
            ':address' => $formatted,
            ':id' => $id
        ]);
        echo "Updated ID $id: $original => $formatted\n";
    }
}
?>

