require 'open-uri'
sig = "zaQ/876CwJMEEmrJqAOYHyEKBXy2s03NDmk+3FsXPr4=".force_encoding('ASCII-8BIT')
puts URI::encode(sig)
puts URI.escape(sig)
