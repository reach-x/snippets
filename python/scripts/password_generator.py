#!/usr/bin/env python3

import random
import string
import secrets

def generate_password(length=16, use_symbols=True, use_numbers=True):
    characters = string.ascii_letters

    if use_numbers:
        characters += string.digits

    if use_symbols:
        characters += string.punctuation

    password = ''.join(secrets.choice(characters) for _ in range(length))
    return password

def generate_memorable_password(word_count=4):
    words = [
        'apple', 'banana', 'cherry', 'dragon', 'elephant', 'forest',
        'garden', 'house', 'island', 'jungle', 'kitchen', 'lemon',
        'mountain', 'notebook', 'ocean', 'planet', 'question', 'river',
        'sunset', 'thunder', 'umbrella', 'village', 'window', 'yellow'
    ]

    selected_words = [secrets.choice(words).capitalize() for _ in range(word_count)]
    number = secrets.randbelow(100)
    symbol = secrets.choice('!@#$%&*')

    password = '-'.join(selected_words) + str(number) + symbol
    return password

def check_password_strength(password):
    score = 0

    if len(password) >= 8:
        score += 1
    if len(password) >= 12:
        score += 1
    if any(c.isupper() for c in password):
        score += 1
    if any(c.islower() for c in password):
        score += 1
    if any(c.isdigit() for c in password):
        score += 1
    if any(c in string.punctuation for c in password):
        score += 1

    strength = ['Very Weak', 'Weak', 'Fair', 'Good', 'Strong', 'Very Strong']
    return strength[min(score, 5)]

if __name__ == '__main__':
    print("Password Generator\n")

    pwd1 = generate_password(16)
    print(f"Random password (16 chars): {pwd1}")
    print(f"Strength: {check_password_strength(pwd1)}\n")

    pwd2 = generate_password(12, use_symbols=False)
    print(f"Alphanumeric password (12 chars): {pwd2}")
    print(f"Strength: {check_password_strength(pwd2)}\n")

    pwd3 = generate_memorable_password(4)
    print(f"Memorable password: {pwd3}")
    print(f"Strength: {check_password_strength(pwd3)}")
