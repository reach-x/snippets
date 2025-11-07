struct User {
  1: i32 id,
  2: string name,
  3: string email
}

service UserService {
  User getUser(1: i32 id),
  list<User> listUsers()
}
