#!/usr/bin/env python3

class StringUtils:
    @staticmethod
    def clean_phone(phone_number):
        import re
        return re.sub(r'[^0-9]', '', phone_number)

    @staticmethod
    def to_snake_case(text):
        import re
        text = re.sub(r'([A-Z])', r'_\1', text).lower()
        return text.lstrip('_')

    @staticmethod
    def to_camel_case(text):
        components = text.split('_')
        return components[0] + ''.join(x.title() for x in components[1:])

    @staticmethod
    def truncate(text, length=50, suffix='...'):
        if len(text) <= length:
            return text
        return text[:length - len(suffix)] + suffix

    @staticmethod
    def slugify(text):
        import re
        text = text.lower().strip()
        text = re.sub(r'[^\w\s-]', '', text)
        text = re.sub(r'[-\s]+', '-', text)
        return text

if __name__ == '__main__':
    print("String Utilities Test\n")

    phone = "1-800-555-1234"
    print(f"Clean phone: {StringUtils.clean_phone(phone)}")

    camel = "userName"
    print(f"To snake_case: {StringUtils.to_snake_case(camel)}")

    snake = "user_name"
    print(f"To camelCase: {StringUtils.to_camel_case(snake)}")

    long_text = "This is a very long text that needs to be truncated"
    print(f"Truncated: {StringUtils.truncate(long_text, 20)}")

    title = "Hello World! This is a Test"
    print(f"Slugified: {StringUtils.slugify(title)}")
