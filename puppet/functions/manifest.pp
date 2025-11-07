# Puppet Manifest Example
# Configuration management

# Variables
$app_name = 'myapp'
$app_version = '1.0.0'
$deploy_user = 'appuser'
$app_dir = "/var/www/${app_name}"
$log_dir = "/var/log/${app_name}"

# Facts (system information)
notify { "Operating System: ${facts['os']['name']}": }
notify { "IP Address: ${facts['networking']['ip']}": }

# Package management
package { 'nginx':
  ensure => installed,
}

package { ['git', 'curl', 'python3', 'python3-pip']:
  ensure => present,
}

# User management
user { $deploy_user:
  ensure     => present,
  home       => "/home/${deploy_user}",
  shell      => '/bin/bash',
  managehome => true,
}

# Group management
group { 'developers':
  ensure => present,
  gid    => 1001,
}

# File resource
file { $app_dir:
  ensure  => directory,
  owner   => $deploy_user,
  group   => $deploy_user,
  mode    => '0755',
  require => User[$deploy_user],
}

file { '/etc/myapp/config.conf':
  ensure  => file,
  content => template('myapp/config.conf.erb'),
  owner   => 'root',
  mode    => '0644',
  notify  => Service['myapp'],
}

file { '/etc/nginx/sites-available/myapp':
  ensure  => file,
  source  => 'puppet:///modules/myapp/nginx.conf',
  owner   => 'root',
  mode    => '0644',
  require => Package['nginx'],
  notify  => Service['nginx'],
}

file { '/etc/nginx/sites-enabled/myapp':
  ensure  => link,
  target  => '/etc/nginx/sites-available/myapp',
  require => File['/etc/nginx/sites-available/myapp'],
  notify  => Service['nginx'],
}

# Service management
service { 'nginx':
  ensure    => running,
  enable    => true,
  subscribe => File['/etc/nginx/sites-available/myapp'],
}

service { 'myapp':
  ensure  => running,
  enable  => true,
  require => [
    File[$app_dir],
    Package['python3'],
  ],
}

# Exec resource (run commands)
exec { 'install-pip-requirements':
  command => "/usr/bin/pip3 install -r ${app_dir}/requirements.txt",
  path    => ['/usr/bin', '/usr/local/bin'],
  creates => "${app_dir}/venv",
  require => [
    Package['python3-pip'],
    File["${app_dir}/requirements.txt"],
  ],
  user    => $deploy_user,
}

exec { 'git-clone-app':
  command => "/usr/bin/git clone https://github.com/example/myapp.git ${app_dir}/src",
  path    => ['/usr/bin', '/usr/local/bin'],
  creates => "${app_dir}/src",
  require => [
    Package['git'],
    File[$app_dir],
  ],
  user    => $deploy_user,
}

# Cron jobs
cron { 'backup-database':
  command => '/usr/local/bin/backup.sh',
  user    => 'root',
  hour    => 2,
  minute  => 0,
}

cron { 'cleanup-logs':
  command  => "find ${log_dir} -name '*.log' -mtime +30 -delete",
  user     => $deploy_user,
  hour     => 3,
  minute   => 0,
  weekday  => 0,
  require  => File[$log_dir],
}

# Conditionals
if $facts['os']['family'] == 'Debian' {
  package { 'apt-transport-https':
    ensure => installed,
  }

  exec { 'apt-update':
    command => '/usr/bin/apt-get update',
    refreshonly => true,
  }
} elsif $facts['os']['family'] == 'RedHat' {
  package { 'yum-utils':
    ensure => installed,
  }
}

# Case statement
case $facts['os']['name'] {
  'Ubuntu': {
    $package_manager = 'apt'
    $config_dir = '/etc/apt'
  }
  'CentOS', 'RedHat': {
    $package_manager = 'yum'
    $config_dir = '/etc/yum'
  }
  'Debian': {
    $package_manager = 'apt'
    $config_dir = '/etc/apt'
  }
  default: {
    fail("Unsupported OS: ${facts['os']['name']}")
  }
}

# Unless (negative conditional)
unless $facts['virtual'] == 'docker' {
  service { 'firewalld':
    ensure => running,
    enable => true,
  }
}

# Selectors (inline conditionals)
$web_server = $facts['os']['family'] ? {
  'Debian' => 'apache2',
  'RedHat' => 'httpd',
  default  => 'nginx',
}

# Classes
class myapp {
  package { 'myapp-dependencies':
    ensure => present,
  }

  service { 'myapp':
    ensure => running,
  }
}

# Class with parameters
class myapp::install (
  String $version = '1.0.0',
  String $user = 'appuser',
  Boolean $enable_monitoring = true,
) {
  package { 'myapp':
    ensure => $version,
  }

  if $enable_monitoring {
    include myapp::monitoring
  }
}

# Class inheritance
class myapp::production inherits myapp {
  Service['myapp'] {
    ensure => running,
    enable => true,
  }
}

# Defined types (reusable resources)
define myapp::vhost (
  String $port = '80',
  String $docroot = '/var/www/html',
  String $server_name = $title,
) {
  file { "/etc/nginx/sites-available/${title}":
    ensure  => file,
    content => template('myapp/vhost.erb'),
  }

  file { "/etc/nginx/sites-enabled/${title}":
    ensure  => link,
    target  => "/etc/nginx/sites-available/${title}",
    require => File["/etc/nginx/sites-available/${title}"],
  }
}

# Using defined type
myapp::vhost { 'example.com':
  port        => '443',
  docroot     => '/var/www/example',
  server_name => 'example.com',
}

# Hiera data lookup
$db_password = lookup('database::password', String)
$api_key = lookup('api::key', String, 'first', 'default-key')

# Arrays and hashes
$packages = ['nginx', 'php', 'mysql']
$packages.each |String $pkg| {
  package { $pkg:
    ensure => installed,
  }
}

$vhosts = {
  'site1.com' => {
    'port'    => '80',
    'docroot' => '/var/www/site1',
  },
  'site2.com' => {
    'port'    => '443',
    'docroot' => '/var/www/site2',
  },
}

$vhosts.each |String $name, Hash $config| {
  myapp::vhost { $name:
    * => $config,
  }
}

# Resource collectors
File <| tag == 'config' |>
Package <| tag == 'database' |>

# Virtual resources
@user { 'dbadmin':
  ensure => present,
  tag    => 'database',
}

# Realize virtual resource
realize(User['dbadmin'])

# Facts
notify { "Processor count: ${facts['processors']['count']}": }
notify { "Memory: ${facts['memory']['system']['total']}": }

# Custom facts
# Create file in: lib/facter/custom_fact.rb
# Use with: $facts['custom_fact']

# Functions
$uppercase = upcase('hello')
$joined = join(['a', 'b', 'c'], ',')
$filtered = filter([1,2,3,4,5]) |$x| { $x > 2 }

# Templates (ERB)
# File content: <%= @variable %>
# Iteration: <% @items.each do |item| %>

# Relationships
Package['nginx'] -> File['/etc/nginx/nginx.conf'] ~> Service['nginx']

# Dependency chaining
Package['mysql']
-> File['/etc/mysql/my.cnf']
~> Service['mysql']

# Ordering
File['/etc/config'] {
  before => Service['myapp'],
}

Service['myapp'] {
  require => Package['myapp'],
  notify  => Exec['reload-app'],
}

# Stages
stage { 'pre':
  before => Stage['main'],
}

stage { 'post':
  require => Stage['main'],
}

class { 'apt':
  stage => 'pre',
}

# Node definitions
node 'web1.example.com' {
  include myapp
  include nginx
}

node /^db\d+\.example\.com$/ {
  include postgresql
  include monitoring
}

node default {
  notify { 'Unconfigured node': }
}

# Modules
# module/
#   manifests/
#     init.pp
#     install.pp
#   files/
#   templates/
#   lib/
#     facter/
#     puppet/
#   tests/
#   metadata.json

notify { 'Puppet manifest complete': }
