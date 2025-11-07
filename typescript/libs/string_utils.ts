export class StringUtils {
    static cleanPhone(phoneNumber: string): string {
        return phoneNumber.replace(/[^0-9]/g, '');
    }

    static toSnakeCase(text: string): string {
        return text
            .replace(/([A-Z])/g, '_$1')
            .toLowerCase()
            .replace(/^_/, '');
    }

    static toCamelCase(text: string): string {
        return text.replace(/_([a-z])/g, (match, letter) => letter.toUpperCase());
    }

    static truncate(text: string, length: number = 50, suffix: string = '...'): string {
        if (text.length <= length) {
            return text;
        }
        return text.substring(0, length - suffix.length) + suffix;
    }

    static slugify(text: string): string {
        return text
            .toLowerCase()
            .trim()
            .replace(/[^\w\s-]/g, '')
            .replace(/[-\s]+/g, '-');
    }

    static capitalize(text: string): string {
        return text.charAt(0).toUpperCase() + text.slice(1);
    }

    static reverse(text: string): string {
        return text.split('').reverse().join('');
    }

    static isPalindrome(text: string): boolean {
        const cleaned = text.toLowerCase().replace(/[^a-z0-9]/g, '');
        return cleaned === cleaned.split('').reverse().join('');
    }
}

// Example usage
if (require.main === module) {
    console.log('String Utilities Test\n');

    const phone = '1-800-555-1234';
    console.log(`Clean phone: ${StringUtils.cleanPhone(phone)}`);

    const camel = 'userName';
    console.log(`To snake_case: ${StringUtils.toSnakeCase(camel)}`);

    const snake = 'user_name';
    console.log(`To camelCase: ${StringUtils.toCamelCase(snake)}`);

    const longText = 'This is a very long text that needs to be truncated';
    console.log(`Truncated: ${StringUtils.truncate(longText, 20)}`);

    const title = 'Hello World! This is a Test';
    console.log(`Slugified: ${StringUtils.slugify(title)}`);

    console.log(`Capitalized: ${StringUtils.capitalize('hello')}`);
    console.log(`Reversed: ${StringUtils.reverse('hello')}`);
    console.log(`Is palindrome: ${StringUtils.isPalindrome('racecar')}`);
}
