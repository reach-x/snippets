#!/usr/bin/env ruby

require 'net/http'
require 'json'
require 'uri'

url = URI('https://api.github.com/users/github')

begin
  response = Net::HTTP.start(url.host, url.port, use_ssl: true) do |http|
    request = Net::HTTP::Get.new(url)
    request['User-Agent'] = 'Ruby HTTP Client'
    http.request(request)
  end

  puts "Status Code: #{response.code}"

  if response.code == '200'
    data = JSON.parse(response.body)
    puts "\nUser: #{data['name']}"
    puts "Bio: #{data['bio']}"
    puts "Public repos: #{data['public_repos']}"
  end
rescue StandardError => e
  puts "Error: #{e.message}"
end
