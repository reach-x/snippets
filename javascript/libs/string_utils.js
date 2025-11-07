#!/usr/bin/env node

class StringUtils {
    static cleanPhone(phoneNumber) {
        return phoneNumber.replace(/[^0-9]/g, '');
    }

    static toSnakeCase(text) {
        return text
            .replace(/([A-Z])/g, '_$1')
            .toLowerCase()
            .replace(/^_/, '');
    }

    static toCamelCase(text) {
        return text.replace(/_([a-z])/g, (match, letter) => letter.toUpperCase());
    }

    static truncate(text, length = 50, suffix = '...') {
        if (text.length <= length) {
            return text;
        }
        return text.substring(0, length - suffix.length) + suffix;
    }

    static slugify(text) {
        return text
            .toLowerCase()
            .trim()
            .replace(/[^\w\s-]/g, '')
            .replace(/[-\s]+/g, '-');
    }

    static capitalize(text) {
        return text.charAt(0).toUpperCase() + text.slice(1);
    }

    static reverse(text) {
        return text.split('').reverse().join('');
    }
}

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
}

module.exports = StringUtils;
