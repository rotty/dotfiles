#!/usr/bin/env ruby

# mess tools
# deal with messy home directories

require 'fileutils'
require 'date'

MESSDIR = ENV["MESSDIR"] ||= File.expand_path("~/mess")
MESSVERSION = "0.1"

FU = FileUtils #::DryRun

unless ARGV.empty?
  warn <<EOF.strip
mess.rb #{MESSVERSION}
Copyright (C) 2005  Christian Neukirchen <chneukirchen@gmail.com>
EOF
else
  d = Date.today
  current = File.join(MESSDIR, d.year.to_s, "%02d" % d.cweek)
  current_link = File.join(MESSDIR, "current")

  unless File.directory? current
    FU.mkdir_p current
    warn "Created messdir #{current}"
  end

  if File.exist?(current_link) && !File.symlink?(current_link)
    warn "`#{current_link}' is not a symlink, something is wrong."
  else
    if File.expand_path(current_link) != File.expand_path(current)
      FU.rm_f current_link
      FU.ln_s current, current_link
    end
  end
  puts current_link
end
