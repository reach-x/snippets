function identity<T>(arg: T): T {
    return arg;
}

console.log(identity<number>(42));
console.log(identity<string>('Hello'));
console.log(identity<boolean>(true));

class Box<T> {
    private value: T;

    constructor(value: T) {
        this.value = value;
    }

    getValue(): T {
        return this.value;
    }

    setValue(value: T): void {
        this.value = value;
    }
}

const numberBox = new Box<number>(123);
console.log(`Number box: ${numberBox.getValue()}`);

const stringBox = new Box<string>('Hello TypeScript');
console.log(`String box: ${stringBox.getValue()}`);

interface KeyValuePair<K, V> {
    key: K;
    value: V;
}

const pair: KeyValuePair<string, number> = {
    key: 'age',
    value: 30
};

console.log(`Pair: ${JSON.stringify(pair)}`);
