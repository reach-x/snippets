enum Color {
    Red,
    Green,
    Blue
}

console.log(`Color.Red: ${Color.Red}`);
console.log(`Color.Green: ${Color.Green}`);
console.log(`Color.Blue: ${Color.Blue}`);

enum Status {
    Active = 'ACTIVE',
    Inactive = 'INACTIVE',
    Pending = 'PENDING'
}

function getStatusMessage(status: Status): string {
    switch (status) {
        case Status.Active:
            return 'The item is active';
        case Status.Inactive:
            return 'The item is inactive';
        case Status.Pending:
            return 'The item is pending';
        default:
            return 'Unknown status';
    }
}

console.log(getStatusMessage(Status.Active));
console.log(getStatusMessage(Status.Pending));

enum Permission {
    Read = 1,
    Write = 2,
    Execute = 4
}

const userPermissions = Permission.Read | Permission.Write;
console.log(`User has read permission: ${(userPermissions & Permission.Read) !== 0}`);
console.log(`User has execute permission: ${(userPermissions & Permission.Execute) !== 0}`);
