#!/usr/bin/env ruby

module StringUtils
  def self.clean_phone(phone_number)
    phone_number.gsub(/[^0-9]/, '')
  end

  def self.to_snake_case(text)
    text.gsub(/([A-Z])/, '_\1').downcase.sub(/^_/, '')
  end

  def self.to_camel_case(text)
    text.split('_').map.with_index { |part, i| i.zero? ? part : part.capitalize }.join
  end

  def self.truncate(text, length = 50, suffix = '...')
    return text if text.length <= length
    text[0...(length - suffix.length)] + suffix
  end

  def self.slugify(text)
    text.downcase.strip.gsub(/[^\w\s-]/, '').gsub(/[-\s]+/, '-')
  end

  def self.capitalize_words(text)
    text.split.map(&:capitalize).join(' ')
  end
end

if __FILE__ == $PROGRAM_NAME
  puts "String Utilities Test\n\n"

  phone = '1-800-555-1234'
  puts "Clean phone: #{StringUtils.clean_phone(phone)}"

  camel = 'userName'
  puts "To snake_case: #{StringUtils.to_snake_case(camel)}"

  snake = 'user_name'
  puts "To camelCase: #{StringUtils.to_camel_case(snake)}"

  long_text = 'This is a very long text that needs to be truncated'
  puts "Truncated: #{StringUtils.truncate(long_text, 20)}"

  title = 'Hello World! This is a Test'
  puts "Slugified: #{StringUtils.slugify(title)}"

  puts "Capitalized: #{StringUtils.capitalize_words('hello world')}"
end
