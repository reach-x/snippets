MATCH (p:Person)-[:KNOWS]->(friend)
WHERE p.name = "Alice"
RETURN friend.name
