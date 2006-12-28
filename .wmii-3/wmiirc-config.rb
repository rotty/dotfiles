# {{{ ======== ruby-wmii CONFIGURATION BEGINS HERE ==============
 
# Set the log level
# It defaults to Logger::INFO.
# Set to Logger::DEBUG for extra verbosity.
#LOGGER.level = Logger::DEBUG

# programs to run when wmiirc starts
# one per line, they're run sequentially right before the main loop begins
START_PROGS = <<EOF
xsetroot -solid '#333333'
EOF

# {{{ WM CONFIGURATION
WMII::Configuration.define do
  border      1
  font        "fixed"
  selcolors   '#FFFFFF #248047 #147027'
  normcolors  '#4D4E4F #DDDDAA #FFFFCC'
  colmode     'default'
  colwidth    0
  grabmod     'Mod1'
  rules <<EOF
/Kdict.*/ -> dict
/XMMS.*/ -> ~
/Gimp.*/ -> ~
/MPlayer.*/ -> ~
/XForm.*/ -> ~
/XSane.*/ -> ~
/fontforge.*/ -> ~
/.*/ -> !
/.*/ -> 1
EOF

  # Translate the following names in the on_key and use_binding definitions.
  key_subs  :MODKEY  => :Mod4,
            :MODKEY2 => :Mod5,
            :LEFT    => :Left,
            :RIGHT   => :Right,
            :UP      => :Up,
            :DOWN    => :Down


  # Constant used by the intellisort tag selection mechanism
  # set it to   0.0 <= value <= 1.0
  # Lower values make recent choices more likely (modified first order
  # markovian process with exponential decay):
  # 0.0 means that only the last transition counts (all others forgotten)
  # 1.0 means that the probabilities aren't biased to make recent choices more
  #     likely
  view_history_decay 0.8

  # Favor the view we came from in intellisort.
  # 1.0: that view is the first choice
  # 0.0: that view comes after all views with non-zero transition probability,
  #      but before all views we haven't yet jumped to from the current one
  view_history_prev_bias 0.4

# {{{ Plugin config
  
  # Uncomment and change to override default on_click actions for the status
  # bar
  #plugin_config["standard:status"]["left_click_action"] = lambda{ system "xeyes" }
  #plugin_config["standard:status"]["right_click_action"] = lambda{ system "xeyes" }
  #plugin_config["standard:status"]["middle_click_action"] = lambda{ system "xeyes" }
  
  plugin_config["standard:status"]["refresh_time"] = 1
  
  # Uncomment and change to override default text
  #currload = nil
  #Thread.new{ loop { currload = `uptime`.chomp.sub(/.*: /,"").gsub(/,/,""); sleep 10 } }
  #plugin_config["standard:status"]["text_proc"] = lambda do
  #  "#{Time.new.strftime("%d/%m/%Y %X %Z")} #{currload}"
  #end

  plugin_config["standard"]["x-terminal-emulator"] = "x-terminal-emulator"

  plugin_config["standard:actions"]["history_size"] = 3  # set to 0 to disable
  plugin_config["standard:programs"]["history_size"] = 5 # set to 0 to disable

  plugin_config["standard:volume"]["mixer"] = "Master"
  
  plugin_config["standard:mode"]["mode_toggle_keys"] = ["MODKEY2-space"]

  plugin_config["standard:battery-monitor"]["statefile"] = 
      '/proc/acpi/battery/BAT0/state'
  plugin_config["standard:battery-monitor"]["infofile"] =
      '/proc/acpi/battery/BAT0/info'
  plugin_config["standard:battery-monitor"]["low"] = 5
  plugin_config["standard:battery-monitor"]["low_action"] =
      'echo "Low battery" | xmessage -center -buttons quit:0 -default quit -file -'
  plugin_config["standard:battery-monitor"]["critical"] = 1
  plugin_config["standard:battery-monitor"]["critical_action"] =
      'echo "Critical battery" | xmessage -center -buttons quit:0 -default quit -file -'

  # Allows you to override the default internal actions and define new ones:
  #plugin_config["standard:actions"]["internal"].update({
  #  "screenshot" => nil,    # remove default screenshot action
  #  "google" => lambda do |wmii, *selection|
  #    require 'cgi'
  #    if selection && !selection.empty?
  #      selection = CGI.escape(selection.join(" "))
  #    else
  #      selection = CGI.escape(%!#{`wmiipsel`.strip}!)
  #    end
  #    url = "http://www.google.com/search?q=#{selection}"
  #    case browser = ENV["BROWSER"]
  #    when nil: system "wmiisetsid /etc/alternatives/x-www-browser '#{url}' &"
  #    else system "wmiisetsid #{browser} '#{url}' &"
  #    end
  #  end,
  #  "foo" => lambda do |wmii, *args|
  #    IO.popen("xmessage -file -", "w"){|f| f.puts "Args: #{args.inspect}"; f.close_write }
  #  end
  #})

#{{{ Import bindings and bar applets
  from "standard"  do
    use_bar_applet "volume", 999
    use_bar_applet "mode", 900
    use_bar_applet "status", 100
    #use_bar_applet "cpuinfo", 150
    #use_bar_applet "mpd", 110
    #use_bar_applet "battery-monitor"

    use_binding "dict-lookup"
    use_binding "execute-program-with-tag"
    use_binding "execute-action"
    use_binding "execute-program"
    (0..9).each{|k| use_binding "numeric-jump-#{k}"  }
    use_binding "tag-jump"
    use_binding "retag"
    use_binding "retag-jump"
    use_binding "namespace-retag"
    use_binding "namespace-retag-jump"
    (('a'..'z').to_a+('0'..'9').to_a).each{|k| use_binding "letter-jump-#{k}" }
    (0..9).each{|k| use_binding "numeric-retag-#{k}" }
    (('a'..'z').to_a+('0'..'9').to_a).each{|k| use_binding "letter-retag-#{k}" }
    use_binding "move-prev"
    use_binding "move-next"
    use_binding "namespace-move-prev"
    use_binding "namespace-move-next"
    use_binding "history-move-forward"
    use_binding "history-move-back"

    use_binding "bookmark"
    use_binding "bookmark-open"
  end
  
  # {{{ del.icio.us bookmark import
  #plugin_config["standard:bookmark"]["del.icio.us-user"] = 'myusername'
  #plugin_config["standard:bookmark"]["del.icio.us-password"] = 'mypass'
  
  ## WORD OF CAUTION! 
  ## Before setting the sync mode to :bidirectional, make sure
  ## that your bookmarks.txt file contains all the bookmarks you want to keep,
  ## because all the del.icio.us bookmarks not listed there will be deleted!
  ## You can import your del.icio.us bookmarks by setting it to
  ## :unidirectional and reloading wmiirc ("ALT-a wmiirc" by default).
  ## Allow some time for the bookmarks to be downloaded (wait until you see
  ## "Done importing bookmarks from del.icio.us." in
  ## $HOME/.wmii-3/wmiirc.log). You can then change the mode to :bidirectional
  ## and reload wmiirc. From that point on, the bookmark lists will be
  ## synchronized, so local modifications will be propagated to del.icio.us,
  ## and if you remove a bookmark locally it will also be deleted on
  ## del.icio.us.
  #plugin_config["standard:bookmark"]["del.icio.us-mode"] = :bidirectional
  #plugin_config["standard:bookmark"]["del.icio.us-share"] = true

  ## Sets the encoding used to:
  #  * store the bookmark descriptions in bookmarks.txt
  #  * present choices through wmiimenu
  # Please make sure your bookmarks.txt uses the appropriate encoding before
  # setting the next line. If you had already imported bookmarks from
  # del.icio.us, they will be stored UTF-8, so you might want to
  #   recode utf-8..NEW_ENCODING bookmarks.txt
  #
  # If left to nil, bookmarks imported from del.icio.us will be in UTF-8, and
  # those created locally will be in the encoding specified by your locale.
  #plugin_config["standard:bookmark"]["encoding"] = 'KOI8-R'

  # Allows you to override the default bookmark protocols and define new ones:
  #plugin_config["standard:bookmark"]["protocols"].update({
  #  'http' => nil,    # remove default http protocol
  #  'ssh' => {
  #    :open_urls => lambda do |wmii,bms|
  #      term = wmii.plugin_config["standard"]["x-terminal-emulator"] || "xterm"
  #      bms.each do |bm|
  #        uri = bm[:uri]
  #        ssh_host = uri.host
  #        ssh_host = "#{uri.user}@" + ssh_host unless uri.user.nil?
  #        ssh_port = "-p #{uri.port}" unless uri.port.nil?
  #        system "wmiisetsid #{term} -T '#{bm[:bm].url}' -e 'ssh #{ssh_host} #{ssh_port} || read' &"
  #      end
  #    end,
  #    :get_title => lambda do |wmii,uri|
  #      title = uri.host
  #      title = "#{uri.user}@" + title unless uri.user.nil?
  #      title << ":#{uri.port.to_s}" unless uri.port.nil?
  #      title
  #    end
  #  },
  #  'pdf' => {
  #    :open_urls => lambda do |wmii,bms|
  #      bms.each do |bm|
  #        path = URI.unescape(bm[:uri].path)
  #        LOGGER.info "Opening #{path} with xpdf."
  #        system "wmiisetsid xpdf '#{path}' &"
  #      end
  #    end,
  #    :get_title => lambda do |wmii,uri|
  #      fname = File.basename(URI.unescape(uri.to_s)).gsub(/\.\S+$/,"")
  #      [fname, fname.downcase, fname.capitalize]
  #    end
  #  }
  #})

  # {{{ Click on view bars
  on_barclick(/./, MOUSE_BUTTON_LEFT){|name,| view name}
  on_barclick(/./, MOUSE_BUTTON_RIGHT){|name,| view name}

  # {{{ Tag all browser instances as 'web' in addition to the current tag
  browsers = %w[Firefox Konqueror]
  browser_re = /^#{browsers.join("|")}/
  on_createclient(condition{|c| browser_re =~ read("/client/#{c}/class")}) do |cid|
    write("/client/#{cid}/tags", normalize(read("/client/#{cid}/tags") + "+web"))
  end

#{{{ Simpler key bindings --- not defined in plugins
  on_key("MODKEY-LEFT"){ write "/view/ctl", "select prev" }
  on_key("MODKEY-RIGHT"){ write "/view/ctl", "select next" }
  on_key("MODKEY-DOWN"){ write "/view/sel/ctl", "select next" }
  on_key("MODKEY-UP"){ write "/view/sel/ctl", "select prev" }
  on_key("MODKEY-space"){ write "/view/ctl", "select toggle" }
  on_key("MODKEY-d"){ write "/view/sel/mode", "default" }
  on_key("MODKEY-s"){ write "/view/sel/mode", "stack" }
  on_key("MODKEY-m"){ write "/view/sel/mode", "max" }
  on_key("MODKEY-f"){ write "/view/0/sel/geom", "0 0 east south" }
  on_key("MODKEY-i"){ write "/view/sel/sel/geom", "+0 +0 +0 +48" }
  on_key("MODKEY-Shift-i"){ write "/view/sel/sel/geom", "+0 +0 +0 -48" }
  on_key("MODKEY-Return") do 
    term = plugin_config["standard"]["x-terminal-emulator"] || "xterm"
    system "wmiisetsid #{term} &"
  end
  on_key("MODKEY-Shift-LEFT"){ write "/view/sel/sel/ctl", "sendto prev" }
  on_key("MODKEY-Shift-RIGHT"){ write "/view/sel/sel/ctl", "sendto next" }
  on_key("MODKEY-Shift-DOWN"){ write "/view/sel/sel/ctl", "swap down" }
  on_key("MODKEY-Shift-UP"){ write "/view/sel/sel/ctl", "swap up" }
  on_key("MODKEY-Shift-space"){ write "/view/sel/sel/ctl", "sendto toggle" }
  on_key("MODKEY-Shift-c"){ write "/view/sel/sel/ctl", "kill" }
  on_key("MODKEY-r"){ view prev_view }
  on_key("MODKEY-Control-LEFT") { write "/view/sel/sel/ctl", "swap prev" }
  on_key("MODKEY-Control-RIGHT"){ write "/view/sel/sel/ctl", "swap next" }

  
# {{{ ======== CONFIGURATION ENDS HERE ==============
end
