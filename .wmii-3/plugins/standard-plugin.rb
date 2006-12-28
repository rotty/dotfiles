# Copyright (c) 2006 Mauricio Fernandez <mfp@acm.org> 
#      http://eigenclass.org/hiki.rb?wmii+ruby
# Licensed under the same terms as Ruby (see LICENSE).
#
# ===========================================================================
#
#{{{ Core bindings and applets defined as the "standard" plugin
# 
# This file must be placed in $HOME/.wmii-3/plugins.
# 
# It will be overwritten when upgrading ruby-wmii, so modifications will be
# lost!
#
# If you want to change a standard binding/applet, either:
# * send a patch to <mfp@acm.org> (put wmii in the subject)
# * copy the relevant code to another file under $HOME/.wmii-3/plugins
#   and edit as needed. Don't forget to change the namespace, e.g.
#     Plugin.define "my-address@isp.com" do
#        .... paste the code here & change it ...
#     end
#   you'll also have to change wmiirc-config.rb to import the new
#   bindings/applets. For example, if you want to use new definitions of
#   the "retag" binding and the "volume" applet, comment 
#       use_binding "retag"
#       use_bar_applet "volume"
#   in the   from "standard" do .... end   area, and add
#     from "my-address@isp.com" do
#       use_binding "retag"
#       use_bar_applet "volume"
#     end
#
# Read the top of wmiirc or type Alt-a -> config-help for more information on
# plugins.
#
# The standard plugin is updated regularly with new functionality and
# *bugfixes*. You cannot benefit from them automatically on upgrade if you
# copy the code and modify the definitions under your own namespace, though.
# ===========================================================================

Plugin.define "standard"  do
  author '"Mauricio Fernandez" <mfp@acm.org>'

  #{{{ Volume control
  bar_applet("volume", 990) do |wmii, bar|
    mixers = wmii.plugin_config["standard:volume"]["mixer"] || ["Master"]
    mixers = [mixers] if !(Array === mixers)
    update_volume = lambda do |increment|
      if mixers.empty?
        bar.data = "VOL OFF"
        return
      end
      sign = increment < 0 ? "-" : "+"
      status = ''
      mixers.reverse.each do |mixer| # show status of first mixer in list
        status = `amixer set "#{mixer},0" #{increment.abs}#{sign}`
      end
      volume = status [/\[(\d+%)\]/]
      volume = "OFF" if status[/\[off\]/]
      bar.data = "VOL #{volume}"
    end
    Thread.new{ loop { update_volume[0]; sleep 10 } }

    term = wmii.plugin_config["standard"]["x-terminal-emulator"] || "xterm"
    bar.on_click(MOUSE_SCROLL_UP){ update_volume[+1] }
    bar.on_click(MOUSE_SCROLL_DOWN){ update_volume[-1] }
    bar.on_click(MOUSE_BUTTON_LEFT) do
      handler = wmii.on_createclient do |cid|
        wmii.write("/view/sel/sel/ctl", "sendto 0")
        wmii.write("/view/sel/sel/geom", "0 100 east south-100")
        wmii.unregister handler
      end
      system "wmiisetsid #{term} -e alsamixer &"
    end
    bar.on_click(MOUSE_BUTTON_RIGHT) do
      action = case `amixer get "#{mixers.first},0"`
      when /\[off\]/: 'unmute'
      when /\[on\]/ : 'mute'
      end
      mixers.each do |mixer|
        `amixer set "#{mixer},0" #{action}`
      end
      update_volume[0]
    end
  end

  #{{{ Modal keybindings: raw/normal modes.
  bar_applet("mode", 980) do |wmii, bar|
    raw_mode = false
    saved_keys = nil
    keys = wmii.plugin_config["standard:mode"]["mode_toggle_keys"] || ["MODKEY2-space"]
    h = wmii.on_key(*keys) do
      case raw_mode
      when true
        wmii.write("/def/keys", saved_keys)
        raw_mode = false
        bar.data = "-N-"
        LOGGER.info "Entering NORMAL mode."
      when false
        saved_keys = wmii.read("/def/keys")
        wmii.write("/def/keys", h.key)
        raw_mode = true
        bar.data = "-R-"
        LOGGER.info "Entering RAW mode."
      end
    end
    bar.data = "-N-"
  end

  #{{{ Selection + temporary view
  bar_applet("temporary-selection", 985) do |wmii, bar|
    wmii.on_clientfocus do |cid|
      view = wmii.curr_view
      if wmii.read("/client/#{cid}/tags").split(/\+/).include? "#{view}:tmp"
        bar.data = "SEL"
      else
        bar.data = "   "
      end
    end
    wmii.on_key(*([wmii.plugin_config["standard:temporary-selection"]["select-keys"] ||
                   ["MODKEY-x"]]).flatten) do |wmii, |
      old_tags = wmii.curr_client_tags
      case curr = wmii.curr_view
      when /:tmp/
        wmii.retag_curr_client("-#{curr}")
      else
        tmpview = "#{curr}:tmp"
        if old_tags.include? tmpview
          wmii.retag_curr_client("-#{tmpview}")
          bar.data = "   "
        else
          wmii.retag_curr_client("+#{tmpview}")
          bar.data = "SEL"
        end
      end
    end
    wmii.on_key(*([wmii.plugin_config["standard:temporary-selection"]["destroy-keys"] ||
                   ["MODKEY-Shift-x"]]).flatten) do |wmii, |
      ids = lambda{|txt| txt.to_a.map{|x| x.split(/\s+/).last}.select{|x| /\A\d+\z/ =~ x} }
      view = wmii.curr_view.gsub(/:tmp/,"")
      wmii.view view
      ids[wmii.read("/client")].each do |cid|
          ctags_file = "/client/#{cid}/tags"
          old_tags = wmii.read(ctags_file)
          new_tags = wmii.normalize(old_tags.split(/\+/) - ["#{view}:tmp"])
          wmii.write(ctags_file, new_tags) if old_tags != new_tags
      end
      bar.data = "   "
    end
  end

# {{{ Dictionary
  bar_applet("dict", 880, "DICT") do |wmii, bar|
    dict_ask_and_define = lambda do
      Thread.new do
        wmii.wmiimenu([]) do |phrase|
          system "dcop kdict KDictIface definePhrase '#{phrase}'"
        end.value  # block until we get the word
        wmii.set_curr_view "dict" unless wmii.curr_view == "dict"
      end
    end
    wmii.on_key("MODKEY-Control-d"){ dict_ask_and_define.call }
    bar.on_click(MOUSE_BUTTON_LEFT){ dict_ask_and_define.call }
    bar.on_click(MOUSE_BUTTON_RIGHT){ wmii.set_curr_view "dict" }
  end

# {{{ Battery monitor
# Originally by Wael Nasreddine <wael@phoenixlinux.org>.
  bar_applet("battery-monitor", 950) do |wmii, bar|
    statefile = wmii.plugin_config["standard:battery-monitor"]["statefile"] ||
      '/proc/acpi/battery/BAT0/state'
    infofile = wmii.plugin_config["standard:battery-monitor"]["infofile"] ||
      '/proc/acpi/battery/BAT0/info'
    low = wmii.plugin_config["standard:battery-monitor"]["low"] || 5
    low_action = wmii.plugin_config["standard:battery-monitor"]["low-action"] ||
      'echo "Low battery" | xmessage -center -buttons quit:0 -default quit -file -'
    critical = wmii.plugin_config["standard:battery-monitor"]["critical"] || 1
    critical_action = wmii.plugin_config["standard:battery-monitor"]["critical-action"] ||
      'echo "Critical battery" | xmessage -center -buttons quit:0 -default quit -file -'
    warned_low = false
    warned_critical = false
    Thread.new do
      loop do
        batt = IO.readlines(statefile)
        battinfo = IO.readlines(infofile)
        battpresent = battinfo[0].gsub(/.*:\s*/,'').chomp
        if battpresent == "yes"
          batt_percent = ((batt[4].gsub(/.*:\s*/,'').chomp.chomp("mAh").to_f / battinfo[2].gsub(/.*:\s*/,'').chomp.chomp(" mAh").to_f ) * 100).to_i
          batt_state = batt[2].gsub(/.*:\s*/,'').chomp
          # Take action in case battery is low/critical
          if batt_state == "discharging" && batt_percent <= critical
            unless warned_critical
              LOGGER.info "Warning about critical battery."
              system("wmiisetsid #{critical_action} &")
              warned_critical = true
            end
          elsif batt_state == "discharging" && batt_percent <= low
            unless warned_low
              LOGGER.info "Warning about low battery."
              system("wmiisetsid #{low_action} &")
              warned_low = true
            end
          else
            warned_low = false
            warned_critical = false
          end
          # If percent is 100 and state is discharging then
          # the battery is full and not discharging.
          batt_state = "=" if batt_state == "charged" || ( batt_state == "discharging" && batt_percent >= 97 )
          batt_state = "^" if batt_state == "charging"
          batt_state = "v" if batt_state == "discharging"
          text = "#{batt_state} #{batt_percent} #{batt_state}"
          bar.data = text
        else
          bar.data = "N/A"
        end
        sleep 2
      end
    end
  end

  # {{{ MPD Bar
  # Originally  by Wael Nasreddine <wael@phoenixlinux.org>.
  bar_applet("mpd", 100) do |wmii, bar|
    require 'mpd'
    mpd_do_action = lambda do |action, *args|
      Thread.new do
        begin
          mpd = MPD.new
          r = mpd.__send__(action, *args)
          LOGGER.info "MPD #{action}"
          r
        ensure
          mpd.close
        end
      end
    end
    mpdserv = MPD.new
    update_bar = lambda do
      mpdserv_status = mpdserv.status["state"]
      case mpdserv_status
      when 'play' : text = ">>: "; show_info = true
      when 'pause': text = "||: "; show_info = true
      else show_info = false
      end
      if show_info
        title = mpdserv.strf("%t")[0..(wmii.plugin_config["standard:mpd"]["title_maxlen"] || -1)]
        author = mpdserv.strf("%a")[0..(wmii.plugin_config["standard:mpd"]["author_maxlen"] || -1)]
        bar.data = text + "#{author} - #{title} " + mpdserv.strf("(%e/%l)")
      else   # Player is stopped or connection not yet initialized...
        bar.data = "[]: NOT PLAYING"
      end
    end
    # Initialize MPD status
    Thread.new do
      loop{ begin; update_bar.call; rescue Exception; end; sleep 1 }
    end
    bar.on_click(MOUSE_SCROLL_UP)  { mpd_do_action[:previous] }
    bar.on_click(MOUSE_SCROLL_DOWN){ mpd_do_action[:next] }
    bar.on_click(MOUSE_BUTTON_LEFT) do
      Thread.new do
        begin
          mpd = MPD.new
          mpdserv_status = mpd.status
        ensure 
          mpd.close rescue nil
        end
        case mpdserv_status["state"]
        when "play":           mpd_do_action[:pause]
        when "pause", "stop" : mpd_do_action[:play]
        end
      end
    end
    bar.on_click(MOUSE_BUTTON_RIGHT) do
      mpd_handle = wmii.on_createclient do |cid|
        wmii.write("/view/sel/sel/ctl", "sendto 0")
        wmii.write("/view/sel/sel/geom", "400 0 center+200 south")
        wmii.unregister mpd_handle
      end
      wmii.write("/view/ctl", "select toggle")
      term = wmii.plugin_config["standard"]["x-terminal-emulator"] || "xterm"
      system "wmiisetsid #{term} -e ncmpc &"
    end
  end

# # {{{ CPU info
  bar_applet("cpuinfo", 800) do |wmii, bar|
    Thread.new do
      loop do
        cpuinfo = IO.readlines("/proc/cpuinfo")[6].split[-1].sub(/\..*$/,'')
        bar.data = cpuinfo.chomp + " Mhz"
        sleep 5
      end
    end
  end

# {{{ Status bar
  bar_applet("status", 0, "STATUS BAR --- init") do |wmii, bar|
    Thread.new do
      text_proc = wmii.plugin_config["standard:status"]["text_proc"]
      unless text_proc
        currload = nil
        Thread.new{ loop { currload = `uptime`.chomp.sub(/.*: /,"").gsub(/,/,""); sleep 10 } }
        text_proc = lambda { "#{Time.new.strftime("%d/%m/%Y %X %Z")} #{currload}" }
      end
      loop do
        bar.data = text_proc.call
        sleep(wmii.plugin_config["standard:status"]["refresh_period"] || 1)
      end
    end

    xmessagebox = "xmessage -center -buttons quit:0 -default quit -file -"
    term = wmii.plugin_config["standard"]["x-terminal-emulator"] || "xterm"
    fl = lambda{ wmii.write "/view/ctl", "select 0" }
    toggle_fl = lambda{ sleep 2; wmii.write "/view/ctl", "select toggle" }
    left_action = wmii.plugin_config["standard:status"]["left_click_action"] || 
                  lambda { fl[]; system "tzwatch | wmiisetsid #{xmessagebox} &"; toggle_fl[] }
    right_action = wmii.plugin_config["standard:status"]["right_click_action"] || 
                   lambda { fl[]; system "ncal -y | wmiisetsid #{xmessagebox} &"; toggle_fl[] }
    middle_action = wmii.plugin_config["standard:status"]["middle_click_action"] || 
                   lambda {  fl[]; system "wmiisetsid #{term} -e top &"; toggle_fl[] }
    bar.on_click do |name, button|
      current = wmii.curr_view_index
      case button.to_i
      when MOUSE_BUTTON_LEFT:   left_action.call if left_action
      when MOUSE_BUTTON_MIDDLE: middle_action.call if middle_action
      when MOUSE_BUTTON_RIGHT:  right_action.call if right_action
      when MOUSE_SCROLL_UP
        wmii.set_curr_view(wmii.views[wmii.curr_view_index-1] || wmii.views[-1])
      when MOUSE_SCROLL_DOWN
        wmii.set_curr_view(wmii.views[wmii.curr_view_index+1] || wmii.views[0])
      end
    end
  end

  binding("dict-lookup", "MODKEY-Control-d") do |wmii,|
    LOGGER.debug "dict-lookup called!!!"
    Thread.new do
      wmii.wmiimenu([]) do |phrase|
        system "dcop kdict KDictIface definePhrase '#{phrase}'"
      end.value  # block until we get the word
      wmii.set_curr_view "dict" unless wmii.curr_view == "dict"
    end
  end

#{{{ actions (internal and WMIIRC_HOME/*) (w/ history)
  standard_internal_actions = {
    "browser" => lambda do |wmii, *selection|
      selection = selection[0]
      selection ||= `wmiipsel`.strip
      case browser = ENV["BROWSER"]
      when nil: system "wmiisetsid /etc/alternatives/x-www-browser '#{selection}' &"
      else system "wmiisetsid #{browser} '#{selection}' &"
      end
    end,
    "google" => lambda do |wmii, *selection|
      require 'cgi'
      if selection && !selection.empty?
        selection = CGI.escape(selection.join(" "))
      else
        selection = CGI.escape(%!#{`wmiipsel`.strip}!)
      end
      url = "http://www.google.com/search?q=#{selection}"
      case browser = ENV["BROWSER"]
      when nil: system "wmiisetsid /etc/alternatives/x-www-browser '#{url}' &"
      else system "wmiisetsid #{browser} '#{url}' &"
      end
    end,
    "screenshot" => lambda do |wmii, *base|
      fname = (base[0] || "screenshot") + "000"
      fname.succ! while File.exist?(File.join(ENV["HOME"], "tmp", "#{fname}.png"))
      system("import -window root ~/tmp/#{fname}.png &")
    end,
    "rename-view" => lambda do |wmii, *args|
      unless /./ =~ (new_name = args[0].to_s)
        new_name = wmii.wmiimenu([]).value  # blocking, OK
      end
      old = wmii.curr_view
      wmii.read("/client").each do |line|
        cid = line.split(/\s+/).last
        wmii.write("/client/#{cid}/tags", 
              wmii.read("/client/#{cid}/tags").gsub(/\b#{Regexp.escape(old)}\b/, new_name))
      end
      wmii.view new_name
    end,
    "quit" => lambda do |wmii|
      wmii.write "/event", "Bye"
      wmii.write "/ctl", "quit"
    end,
    "config-help" => lambda do |wmii|
      IO.popen("xmessage -file -", "w"){|f| f.puts WMIIRC_HELP_MESSAGE; f.close_write }
    end
  }

  def_settings "actions/{internal,history,history_size}" do |wmii|
    wmii.plugin_config["standard:actions"]["internal"] ||= {}
    wmii.plugin_config["standard:actions"]["history"] = []
    wmii.plugin_config["standard:actions"]["history_size"] = 5
  end
  
  binding("execute-action", "MODKEY-a") do |wmii,|
    internal_actions = standard_internal_actions.merge(wmii.plugin_config["standard:actions"]["internal"])
    history_size = wmii.plugin_config["standard:actions"]["history_size"]
    remembered = wmii.plugin_config["standard:actions"]["history"]
    actions = (wmii.action_list + internal_actions.keys.map{|x| x.to_s}).sort.uniq
    internal_actions.each_pair{|name, action| actions.delete(name) unless action }
    list = remembered + actions
    result = wmii.wmiimenu(list) do |choice|
      choices = choice.split(/\s+/)
      cmd = choices.first
      if internal_actions.has_key? cmd
        internal_actions[cmd].call(wmii, *choices[1..-1]) if internal_actions[cmd]
      else
        system("wmiisetsid #{WMIIRC_HOME}/#{choice} &") if /^\s*$/ !~ choice
      end
    end
    # use result.value to record the choice in the current process
    Thread.new do
      if cmd = result.value.split(/\s+/).first
        remembered.delete cmd
        remembered.unshift cmd
        LOGGER.debug "plugin/actions: history #{remembered.inspect}"
        remembered.replace remembered[0, history_size]
      end
    end
  end

#{{{ program lists
  standard_programs = {
    "programs" => lambda do
      # Get the standard list of executable files in your $PATH
      @__program_list_last_update ||= Time.at(0)
      @__program_list ||= {}
      path_glob = ENV["PATH"].gsub(/,/, '\\,').tr(":",",")
      return @__program_list if Time.new - @__program_list_last_update < 3600

      @__program_list_last_update = Time.new
      Dir.glob("{#{path_glob}}/*").select do |fname|
        File.file?(fname) && File.executable?(fname)
      end.map{|fname| File.basename(fname)}.uniq.each{|v| @__program_list[v] = v}
      @__program_list
    end
  }

  def_settings "programs/histories" do |wmii|
    wmii.plugin_config["standard:programs"]["histories"] = {}
  end

  def run_program(wmii, *lists)
    programs = {}
    history_name = lists.map{|h| h.keys}.flatten.sort.join("-")
    lists.each{|list| list.each_value{|x| programs.merge!(x.call) } }
    programs.delete_if { |k,v| k =~ /^\s*$/ }
    histories = wmii.plugin_config["standard:programs"]["histories"]
    history = histories[history_name] ||= []
    choice = wmii.wmiimenu(history + programs.keys.sort).value
    if /\S/ =~ choice
      s = choice.clone
      s.sub!(/\s+\S+$/, '') until programs.has_key?(s) || s.empty? || !s[/\s/]
      cmd, args = (programs[s] || s), choice.sub(s, '').strip
      LOGGER.info "Executing #{choice}"
      system("wmiisetsid #{cmd} #{args} &")
      return [choice, history_name]
    else
      return nil
    end
  end

  def_settings "programs/{history_size,histories}" do |wmii|
    wmii.plugin_config["standard:programs"]["history_size"] = 5
    wmii.plugin_config['standard:programs']['histories'] = Hash.new{|h,k| h[k] = []}
  end
  
  def record_history(wmii, entry, history_name)
    history_size = wmii.plugin_config["standard:programs"]["history_size"]
    histories = wmii.plugin_config['standard:programs']['histories']
    history = wmii.plugin_config['standard:programs']['histories'][history_name]
    history.delete entry
    history.unshift entry
    LOGGER.debug "plugin/programs: history #{entry.inspect}"
    history.replace history[0, history_size]
  end

  def_settings "programs/lists" do |wmii|
    wmii.plugin_config["standard:programs"]["lists"] ||= {}
  end

#{{{ programs (w/ history)
  binding("execute-program", "MODKEY-p") do |wmii,|
    Thread.new do 
      entry, history_name = run_program(wmii, standard_programs, wmii.plugin_config["standard:programs"]["lists"])
      record_history(wmii, entry, history_name) if entry
    end
  end

# {{{ Run program with given tag
  binding("execute-program-with-tag", "MODKEY-Control-y") do |wmii,|
    rd, wr = IO.pipe
    result = wmii.wmiimenu(wmii.views_intellisort) do |tag|
      begin
        rd.close
        if /^\s*$/ !~ tag
          choice, history_name = run_program(wmii, standard_programs, 
                                             wmii.plugin_config["standard:programs"]["lists"])
          Marshal.dump([choice, history_name], wr)
        else
          Marshal.dump(nil, wr)
        end
      ensure
        wr.close rescue nil
      end
    end
    wr.close
    # using result.value to perform the view switch in the current process so
    # that the transition table can be updated
    Thread.new do 
      begin
        tag = result.value
        if /^\s*$/ !~ tag
          handler = wmii.on_createclient do |cid|
            LOGGER.info "Moving #{cid} to #{tag}"
            wmii.write("/client/#{cid}/tags", tag)
            wmii.unregister handler
            wmii.view tag
          end
        end
        choice, history_name = Marshal.load(rd.read)
        if choice
          record_history(wmii, choice, history_name)
        else # empty string, spaces only, etc
          wmii.unregister handler
        end
      ensure
        rd.close rescue nil
      end
    end
  end

#{{{ Either move to the given numeric tag, if it exists, or to the 
# (N-last_numeric_tag)th non-numeric tag.
# e.g.   views  1 3 4 mail web
#    ALT-1  => 1
#    ALT-3  => 3
#    ALT-5 => mail
#    ALT-6 => web
  (0..9).each do |key|
    binding("numeric-jump-#{key}", "MODKEY-#{key}") do |wmii,|
      all_views = wmii.views
      num_tags = all_views.grep(/^\d+$/)
      nkey = (key - 1) % 10
      if num_tags.include?(key.to_s)
        wmii.view(key)
      elsif nkey >= (prev_index = (num_tags.last || 0).to_i)
        non_num_tags = all_views - num_tags
        wmii.view non_num_tags[nkey - prev_index]
      end
    end
  end

#{{{ Move to given view, with intelligent history
  binding("tag-jump", "MODKEY-t") do |wmii,|
    Thread.new do
      # do it this way so the current process can update the transition table
      wmii.view wmii.wmiimenu(wmii.views_intellisort - [wmii.curr_view]).value
    end
  end

  binding("retag", "MODKEY-Shift-t") do |wmii,|
    wmii.wmiimenu(wmii.views_intellisort){|new_tag| wmii.retag_curr_client(new_tag) }
  end
  binding("retag-jump", "MODKEY-Shift-r") do |wmii,|
    rd, wr = IO.pipe
    wmii.wmiimenu(wmii.views_intellisort) do |new_tag|
      rd.close
      wmii.retag_curr_client(new_tag)
      wr.puts new_tag
      wr.close
    end
    wr.close
    Thread.new do
      new_tag = rd.gets
      rd.close
      wmii.view new_tag[/(?![+-]).*/]
    end
  end

  binding("namespace-retag", "MODKEY2-Shift-t") do |wmii,|
    wmii.wmiimenu(wmii.views){|new_tag| wmii.retag_curr_client_ns(new_tag) }
  end
  binding("namespace-retag-jump", "MODKEY2-Shift-r") do |wmii,|
    rd, wr = IO.pipe
    result = wmii.wmiimenu(wmii.views) do |new_tag|
      rd.close
      wmii.retag_curr_client_ns(new_tag)
      wr.puts new_tag
      wr.close
    end
    wr.close
    Thread.new do
      subtag = rd.gets
      rd.close
      wmii.view "#{wmii.curr_view[/[^:]+/]}:#{subtag[/(?![+-]).*/]}"
    end
  end

  (('a'..'z').to_a+('0'..'9').to_a).each do |key|
    binding("letter-jump-#{key}", "MODKEY2-#{key}") do |wmii,|
      unless wmii.curr_view[0,1] == key
        wmii.view wmii.views_intellisort.find{|x| x[0,1] == key }
      end
    end
  end
# Retag as specified numeric tag if it exists, or 
# (N-last_numeric_tag)th non-numeric tag.
  (0..9).each do |key|
    binding("numeric-retag-#{key}", "MODKEY-Shift-#{key}") do |wmii,|
      all_views = wmii.views
      num_tags = all_views.grep(/^\d+$/)
      curr_tags = wmii.curr_client_tags
      nkey = (key - 1) % 10
      if num_tags.include? key.to_s or key > all_views.size
        new_tags =  curr_tags.reject{|x| /^\d+$/=~ x } + [key.to_s]
      elsif nkey >= (prev_index = (num_tags.last || 0).to_i)
        non_num_tags = all_views - num_tags
        new_tags = non_num_tags[nkey - prev_index]
      else
        break
      end
      LOGGER.info "Retagging #{curr_tags.inspect} => #{new_tags.inspect}"
      wmii.set_curr_client_tags(new_tags)
    end
  end
# Retag current client using tag starting with key.
# Only the current view is replaced.
  (('a'..'z').to_a+('0'..'9').to_a).each do |key|
    binding("letter-retag-#{key}", "MODKEY2-Shift-#{key}") do |wmii,|
      unless wmii.curr_view[0,1] == key
        curr_tags = wmii.curr_client_tags
        new_view = wmii.views_intellisort.find{|x| x[0,1] == key }
        unless new_view.nil?
          new_tags = curr_tags.reject {|view| view == new_view }
          curr_index = new_tags.index wmii.curr_view
          if curr_index.nil?
            LOGGER.error "curr_index is nil in letter-retag"
            return
          end
          new_tags[curr_index] = new_view
          LOGGER.info "Retagging #{curr_tags.inspect} => #{new_tags.inspect}"
          wmii.set_curr_client_tags(new_tags)
        end
      end
    end
  end


  binding("history-move-forward", "MODKEY-plus"){|wmii,| wmii.view_history_forward }
  binding("history-move-back", "MODKEY-minus"){|wmii,| wmii.view_history_back }
  binding("move-prev", "MODKEY-Control-UP", "MODKEY-comma") do |wmii,|
    wmii.view  wmii.views[wmii.curr_view_index-1] || wmii.views[-1]
  end
  binding("move-next", "MODKEY-Control-DOWN", "MODKEY-period") do |wmii,|
    wmii.view  wmii.views[wmii.curr_view_index+1] || wmii.views[0]
  end
  move_within_namespace = lambda do |wmii, offset|
    namespace = wmii.curr_view[/([^:]+)/]
    candidate_views = wmii.views.grep(/#{Regexp.escape(namespace)}\b/)
    dest = candidate_views[candidate_views.index(wmii.curr_view) + offset]
    dest ||= (offset > 0) ? candidate_views[0] : candidate_views[-1]
    wmii.view dest
  end
  binding("namespace-move-prev", "MODKEY2-Shift-UP", "MODKEY2-comma") do |wmii,|
    move_within_namespace.call(wmii, -1)
  end
  binding("namespace-move-next", "MODKEY2-Shift-DOWN", "MODKEY2-period") do |wmii,|
    move_within_namespace.call(wmii, +1)
  end

end

#{{{ Bookmark manager 
# Defines the following bindings:
#  bookmark        take current X11 primary selection (with wmiipsel), ask
#                  for description (suggests the page title). You can
#                  append tags to the description:
#                    Page about foo :foo :ruby :bar
#                  tags the bookmark as :foo, :ruby and :bar, and sets the
#                  description to "Page about foo"
#  bookmark-open   ask for a bookmark and open it on a new browser window
#                  The possible completions are shown as you type text from
#                  the description. You can refine the selection
#                  successively, entering a bit (e.g. a word) at a time
#                  (append a space if you don't want the first suggestion
#                  to be taken). You can also use any number of the following
#                  conditions (possible completions will only be shown
#                  after you press enter in that case):
#
#                  :tag       only bookmarks tagged with :tag
#                  ~t regexp  bookmarks whose description matches regexp
#                  ~u regexp  bookmarks whose URL matches regexp
#                  ~p regexp  bookmarks whose protocol matches regexp
#                  ~d 2001    bookmarks defined/last used in 2001
#                  ~d jan     bookmarks defined/last used in January
#                  ~d >4d     bookmarks defined/last used over 4 days ago
#                  ~d >4m                                      4 months ago
#                  ~d <4d     bookmarks defined/last used less than 4 days ago
#                  ~d <4m                                           4 months ago
#                  ~d q1      bookmarks defined/last used in the first quarter
#                             (q1..q4)
#                 
#                 Example:
#                   eigen :ruby ~d <3m
#                 returns all bookmarks with "eigen" in the description or the
#                 URL, tagged as :ruby, used/defined in the last 3 months
#                 
#                 There are also some commands that apply to the current list
#                 (they will only be recognized as commands if you don't enter
#                 anything else on the same line):
#
#                 !o          open all the bookmarks
#
#                 Usage example:
#                 
#                  :blog<enter>    to select all the bookmarks tagged as :blog
#                  !o<enter>       to open them all
require 'time'
require 'thread'
class BookmarkManager
  Bookmark = Struct.new(:description, :url, :tags, :date)

  def initialize(filename)
    @filename = filename
    @bookmarks = {}
    @deleted_bookmarks = {}
    @loaded = false
    @mutex = Mutex.new
  end

  def load
    @bookmarks.clear
    IO.foreach(@filename) do |line|
      begin
        desc, url, tags, date = line.chomp.split(/\t/).map{|x| x.strip}
        if @bookmarks[url].nil? && !@deleted_bookmarks[url]
          tags = (tags || "").split(/\s/)
          begin
            date = Time.rfc822(date)
          rescue
            date = Time.new
          end
          bm = Bookmark.new(desc, url, tags, date)
          @bookmarks[url] = bm
        else
          LOGGER.warn "Loading bookmark #{url.inspect}: already loaded, skipping."
        end
      rescue Exception
        # keep parsing other lines
      end
    end rescue nil
    @loaded = true
  end

  # Returns the bookmark corresponding to the given URL, or nil.
  def [](url)
    self.load unless @loaded
    @bookmarks[url]
  end

  # Returns true if it was a new bookmark, false if a bookmark
  # with the same URL exists.
  # If a bookmark with the same URL already exists it is replaced.
  def add_bookmark(desc, url, tags, date)
    self.load unless @loaded
    ret = @bookmarks.include? url
    bm = Bookmark.new(desc, url, tags, date)
    @bookmarks[url] = bm
    @deleted_bookmarks.delete(url)
    not ret
  end

  # Remove bookmark matching the given URL if it exists.
  # Returns true if bookmark was removed otherwise returns false.
  def remove_bookmark(url)
    @deleted_bookmarks[url] = @bookmarks[url] if @bookmarks.has_key?(url)
    not @bookmarks.delete(url).nil?
  end

  def bookmarks
    self.load unless @loaded
    @bookmarks.values
  end

  # Saves the bookmarks to the specified filename. It tries to merge them with
  # the list currently stored in the file. Bookmarks present in the later but
  # missing in +self+ which were not explicitly deleted will also be added to the
  # list and saved.
  def save!(destination = @filename)
      merge!
      tmpfile = @filename + "_tmp_#{Process.pid}"
      File.open(tmpfile, "a") do |f|
        @bookmarks.values.sort_by{|bm| bm.date}.reverse_each do |bm|
          f.puts [bm.description, bm.url, bm.tags.join(" "), bm.date.rfc822].join("\t")
        end
        f.sync
      end
      File.rename(tmpfile, destination) # atomic if on the same FS and fleh
      self.load
  end
  
  def merge!
    IO.foreach(@filename) do |line|
      desc, url, tags, date = line.chomp.split(/\t/).map{|x| x.strip}
      unless @deleted_bookmarks[url] ||
          (@bookmarks[url] &&
           @bookmarks[url].date >= date)
        tags = (tags || "").split(/\s/)
        begin
          date = Time.rfc822(date)
        rescue
          date = Time.new
        end
        add_bookmark(desc, url, tags, date)
      end
    end rescue nil
  end
  private :merge!


  def transaction
    begin
      fh = File.open(@filename + ".lock", "a")
      fh.flock(File::LOCK_EX)
      @mutex.synchronize{ yield(self) }
    ensure
      fh.flock(File::LOCK_UN)
      fh.close
    end
  end

  def satisfy_date_condition?(bookmark, condition)
    date = bookmark.date
    case condition
    when /^q1$/i : date.month >= 12 || date.month <= 4
    when /^q2$/i : date.month >= 3  && date.month <= 7
    when /^q3$/i : date.month >= 6  && date.month <= 10
    when /^q4$/i : date.month >= 9 || date.month <= 1
    when /^\d+$/ : date.year == condition.to_i
    when /^\w+$/ : date.month - 1 == Time::RFC2822_MONTH_NAME.index(condition.capitalize)
    when /^([><])(\d+)([md])/
      sign, units, type = $1, $2.to_i, $3
      multiplier = 3600 * 24
      multiplier *= 30.4375 if type == 'm'
      case sign
      when '<':  Time.new - date <= units * multiplier
      when '>':  Time.new - date >= units * multiplier
      end
    end
  end
  private :satisfy_date_condition?
  
  def refine_selection(expression, choices=self.bookmarks)
    expression = expression.strip
    pieces = expression.split(/\s+/)
    criteria = []
    option_needed = false
    pieces.each do |x|
      case option_needed
      when true:    criteria.last << " #{x}"; option_needed = false
      when false:   criteria << x; option_needed = true if /^~\w/ =~ x 
      end
    end
    choices.select do |bm|
      criteria.all? do |criterion|
        case criterion
        when /~t\s+(\S+)/: Regexp.new($1) =~ bm.description
        when /~u\s+(\S+)/: Regexp.new($1) =~ bm.url
        when /~p\s+(\S+)/: Regexp.new($1) =~ bm.url[%r{^(\S+?):/},1]
        when /~d\s+(\S+)/: satisfy_date_condition?(bm, $1)
        when /:\w+$/     : bm.tags.include?(criterion)
        else bm.description.index(criterion) or bm.url.index(criterion)
        end
      end
    end
  end
end

BOOKMARK_FILE = File.join(ENV["HOME"], ".wmii-3", "bookmarks.txt")
BOOKMARK_REMOTE_FILE = BOOKMARK_FILE + ".remote"
BOOKMARK_AGENT = 'ruby-wmii #{WMIIRC_VERSION} (#{WMIIRC_RELEASE_DATE})'

MISSING_DELICIOUS_AUTH_MSG = <<EOF
Missing del.icio.us user/password.
You must set them in your wmiirc-config.rb as follows:

  plugin_config["standard:bookmark"]["del.icio.us-user"] = 'username'
  plugin_config["standard:bookmark"]["del.icio.us-password"] = 'password'
EOF

require 'uri'
require 'open-uri'
require 'net/http'
require 'net/https'
require 'cgi'
require 'iconv'
require 'resolv-replace'

Socket.do_not_reverse_lookup = true

Plugin.define "standard"  do
  author '"Mauricio Fernandez" <mfp@acm.org>'

  DELICIOUS_ENCODING = 'UTF-8'

  def perform_delicious_request(request, user, pass)
    delicious_address = Resolv.new.getaddress 'api.del.icio.us'
    https = Net::HTTP.new(delicious_address, 443)
    https.use_ssl = true

    xml = https.start do
      req = Net::HTTP::Get.new(request, 'User-Agent' => BOOKMARK_AGENT)
      req.basic_auth(user, pass)
      https.request(req).body
    end
  end
  
  def push_delicious_bookmark(bookmark, user, pass, shared = false, encoding = nil)
    LOGGER.debug "Pushing to del.icio.us: #{bookmark.inspect}"
    desc = encoding.nil? ? bookmark.description :
      Iconv.conv(DELICIOUS_ENCODING, encoding, bookmark.description)
    req_url = '/v1/posts/add?'
    req_url << "url=#{CGI.escape(bookmark.url)}"
    req_url << ";description=#{CGI.escape(desc)}"
    tags = bookmark.tags.map{|x| x.gsub(/^:/,'')}.join(' ')
    req_url << ";tags=#{CGI.escape(tags)}"
    req_url << ";replace=yes;shared=#{shared ? 'yes' : 'no'}"
    date = bookmark.date.clone.utc.strftime('%Y-%m-%dT%H:%M:%SZ')
    req_url << ";dt=#{CGI.escape(date)}"

    perform_delicious_request(req_url, user, pass)
  end

  def delete_delicious_bookmark(url, user, pass)
    LOGGER.debug "Deleting from del.icio.us: #{url.inspect}"
    req_url = '/v1/posts/delete?'
    req_url << "url=#{CGI.escape(url)}"

    perform_delicious_request(req_url, user, pass)
  end

  def download_delicious_bookmarks(user, pass, mode = :full, encoding = nil)
    require 'rexml/document'
    require 'parsedate'
    require 'time'
    LOGGER.debug "Downloading del.icio.us bookmarks, #{mode} mode"
    case mode
    when :full
      req = '/v1/posts/all'
    when :partial
      req = '/v1/posts/recent?count=100'
    end
    xml = perform_delicious_request(req, user, pass)

    elements = REXML::Document.new(xml).elements
    ret = []
    elements.each("posts/post") do |el|
      begin
        desc, url, tags, time = %w[description href tag time].map{|x| el.attributes[x]}
        desc = Iconv.conv(encoding, DELICIOUS_ENCODING, desc) unless encoding.nil?
        desc = desc.gsub(/\s+/, " ").strip
        year, month, day, hour, min, sec, = ParseDate.parsedate(time)
        date = Time.utc(year, month, day, hour, min, sec, 0)
        date.localtime
        if tags == 'system:unfiled'
          tags = []
        else
          tags = tags.split(/\s+/).map{|x| ":#{x}"}
        end

        ret << [desc, url, tags, date]
      rescue Iconv::IllegalSequence
        LOGGER.error "download_delicious_bookmarks, #{url}: iconv error #{$!.class}."
      rescue
        LOGGER.error "download_delicious_bookmarks, #{url}: #{$!}."
      end
    end

    ret
  end

  def get_delicious_update_time(user, pass)
    require 'rexml/document'
    require 'parsedate'
    require 'time'
    LOGGER.debug "Getting del.icio.us last update time"

    req = '/v1/posts/update'
    xml = perform_delicious_request(req, user, pass)
    time = REXML::Document.new(xml).elements['update'].attributes['time']
    year, month, day, hour, min, sec, = ParseDate.parsedate(time)
    Time.utc(year, month, day, hour, min, sec, 0)
  end

  def sync_delicious_bookmarks(wmii, mode, last_update_time = nil)
    require 'fileutils'
    config = wmii.plugin_config["standard:bookmark"]
    user = config["del.icio.us-user"] 
    pass = config["del.icio.us-password"] 
    shared = config.has_key?("del.icio.us-share") ? config["del.icio.us-share"] : false
    encoding = config["encoding"]

    if "#{user}".empty? or "#{pass}".empty?
      raise MISSING_DELICIOUS_AUTH_MSG
    end
    
    LOGGER.info "Sync with del.icio.us, in #{mode} mode, last update time #{last_update_time ? last_update_time : "unknown"}."

    bm_manager = BookmarkManager.new(BOOKMARK_FILE)
    prev_bm_manager =  BookmarkManager.new(BOOKMARK_REMOTE_FILE)
    unless File.exist?(BOOKMARK_REMOTE_FILE)
      # This is the first time we sync with del.icio.us.
      # Download all bookmarks.
      download_delicious_bookmarks(user, pass, :full, encoding).each do |desc, url, tags, date|
        prev_bm_manager.add_bookmark(desc, url, tags, date)
      end
      prev_bm_manager.transaction { |bmanager| bmanager.save! }
      last_update_time = Time.now
    end

    # Form lists of local bookmark changes.
    local_changes = {}
    if mode == :bidirectional
      bm_manager.bookmarks.reject{|bm| bm.url !~ %r[^(http|https|ftp)://]}.each do |bm|
        if prev_bm_manager[bm.url].nil?
          local_changes[bm.url] = {:bm => bm, :change => :add}
        elsif prev_bm_manager[bm.url].description != bm.description ||
            prev_bm_manager[bm.url].tags.sort != bm.tags.sort
          local_changes[bm.url] = {:bm => bm, :change => :modify}
        end
      end
      prev_bm_manager.bookmarks.reject{|bm| bm.url !~ %r[^(http|https|ftp)://]}.each do |bm|
        if bm_manager[bm.url].nil?
          local_changes[bm.url] = {:change => :remove}
        end
      end
    end

    # Form lists of remote bookmark changes.
    remote_changes = {}
    if last_update_time.nil? ||
        last_update_time < get_delicious_update_time(user, pass)
      # The last update time is unknown or
      # the remote bookarks were updated since last sync
      remote_bookmarks = {}
      download_delicious_bookmarks(user, pass, :full, encoding).each do |desc, url, tags, date|
        remote_bookmarks[url] = BookmarkManager::Bookmark.new(desc, url, tags, date)
      end

      remote_bookmarks.values.each do |bm|
        if prev_bm_manager[bm.url].nil?
          remote_changes[bm.url] = {:bm => bm, :change => :add}
        elsif prev_bm_manager[bm.url].description != bm.description ||
            prev_bm_manager[bm.url].tags.sort != bm.tags.sort
          remote_changes[bm.url] = {:bm => bm, :change => :modify}
        end
      end
      prev_bm_manager.bookmarks.reject{|bm| bm.url !~ %r[^(http|https|ftp)://]}.each do |bm|
        if remote_bookmarks[bm.url].nil?
          remote_changes[bm.url] = {:change => :remove}
        end
      end
    end

    (local_changes.keys + remote_changes.keys).uniq.each do |url|
      local_change = local_changes[url].nil? ? :none : local_changes[url][:change]
      remote_change = remote_changes[url].nil? ? :none : remote_changes[url][:change]

      if local_change == :none ||
          (remote_change == :remove && local_change != :add)
        # If no local changes - propagate remote changes.
        # If bookmark was deleted remotely, delete it locally
        # even if it was modified locally.
        LOGGER.debug "Bookmark #{url.inspect}: no local changes, propagating remote changes #{remote_change.inspect}."
        case remote_change
        when :modify, :add
          bm = remote_changes[url][:bm]
          bm_manager.add_bookmark(bm.description, bm.url, bm.tags, bm.date)
        when :remove
          bm_manager.remove_bookmark(url) unless local_change == :remove
        else
          LOGGER.error "sync_delicious_bookmarks: unknown change type #{remote_change.inspect}."
        end
      elsif remote_change == :none ||
          (local_change == :remove && remote_change != :add)
        # If no remote changes - propagate local changes.
        # If bookmark was deleted locally, delete it remotely
        # even if it was modified remotely.
        LOGGER.debug "Bookmark #{url.inspect}: no remote changes, propagating local changes #{local_change.inspect}."
        case local_change
        when :modify, :add
          bm = local_changes[url][:bm]
          push_delicious_bookmark(bm, user, pass, shared, encoding)
        when :remove
          delete_delicious_bookmark(url, user, pass) unless remote_change == :remove
        else
          LOGGER.error "sync_delicious_bookmarks: unknown change type #{local_change.inspect}."
        end
      else
        # If bookmark was modified locally and remotely,
        # propagate the newest version.
        local_bm = local_changes[url][:bm]
        remote_bm = remote_changes[url][:bm]
        if local_bm.date <= remote_bm.date
          LOGGER.debug "Bookmark #{url.inspect}: remote changes are newer, propagating remote changes #{remote_change}."
          bm_manager.add_bookmark(remote_bm.description,
                                  remote_bm.url,
                                  remote_bm.tags,
                                  remote_bm.date)
        else
          LOGGER.debug "Bookmark #{url.inspect}: local changes are newer, propagating local changes #{local_change}."
          push_delicious_bookmark(local_bm, user, pass, shared, encoding)
        end
      end
    end
    bm_manager.transaction {|bmanager| bmanager.save!}
    prev_bm_manager.transaction {FileUtils.cp BOOKMARK_FILE, BOOKMARK_REMOTE_FILE}

    LOGGER.info "Done importing bookmarks from del.icio.us."
  end

  def bookmark_url(wmii,url)
    escaped = false
    begin
      uri = URI.parse url
    rescue
      unless escaped
        escaped = true
        url = URI.escape url
        retry
      end
      LOGGER.error "Failed to bookmark #{URI.unescape(url)}: invalid URI."
      return
    end
    bookmark_protocols = wmii.plugin_config["standard:bookmark"]["protocols"]
    protocol_desc = bookmark_protocols[uri.scheme]
    if protocol_desc.nil?
      user_specified_proto = wmii.wmiimenu(bookmark_protocols.keys.sort).value
      user_specified_proto.strip!
      unless user_specified_proto.empty?
        uri.scheme = user_specified_proto
        # reparse the url after scheme change
        uri = URI.parse uri.to_s
        protocol_desc = bookmark_protocols[uri.scheme]
      end
    end
    unless protocol_desc.nil?
      # If url path is empty, set it to '/' to avoid duplication after
      # sync with del.icio.us.
      uri.path = '/' if uri.path.empty?
      title_variants = nil
      unless protocol_desc[:get_title].nil?
        begin
          timeout 5 do
            title_variants = protocol_desc[:get_title].call(wmii,uri)
          end
        rescue Timeout::Error
          LOGGER.warn "get_title timeout for URL #{uri.to_s}."
        rescue
          LOGGER.warn "get_title exception for URL #{uri.to_s}: #{$!}"
        end
      end
      wmii.wmiimenu(title_variants) do |choice|
        tags = choice[/\s+(:\S+\s*)+$/] || ""
        description = choice[0..-1-tags.size].strip
        if description =~ /\S/
          bm_manager = BookmarkManager.new(BOOKMARK_FILE)
          LOGGER.info "Bookmarking #{uri.to_s}: #{description.inspect}, tags #{tags.inspect}"
          bm_manager.add_bookmark(description, uri.to_s, tags.strip.split(/\s+/), Time.new)
          bm_manager.transaction{|bm| bm.save!}
        end
      end
    else
      LOGGER.error "Failed to bookmark #{uri.to_s}: unknown protocol #{uri.scheme.inspect}."
    end
  end
  
  def_settings("actions/internal") do |wmii|
    hash = wmii.plugin_config["standard:actions"]["internal"] ||= {}
=begin
    import_lambda = lambda do 
      info = download_delicious_bookmarks(wmii.plugin_config["standard:bookmark"]["del.icio.us-user"],
                                          wmii.plugin_config["standard:bookmark"]["del.icio.us-password"],
                                          :full)
      bm = BookmarkManager.new(BOOKMARK_FILE)
      info.each{|desc, url, tags, date| bm.add_bookmark(desc, url, tags, date) }
      bm.save!
      File.delete(BOOKMARK_REMOTE_FILE) rescue nil
      prevbm = BookmarkManager.new(BOOKMARK_REMOTE_FILE)
      info.each{|desc, url, tags, date| prevbm.add_bookmark(desc, url, tags, date) }
      prevbm.save!
    end
    hash.update("del.icio.us-import" => import_lambda)
=end

    hash.update(
    {
      "del.icio.us-sync" => lambda do
        mode = wmii.plugin_config["standard:bookmark"]["del.icio.us-mode"] || :unidirectional
        sync_delicious_bookmarks(wmii, mode)
      end,
      "bookmark-add" => lambda do |wmii|
        # if xclip is not available, the first one will be empty
        options = [`xclip -o -selection clipboard`, `wmiipsel`].reject {|x| x.strip.empty?}
        wmii.wmiimenu(options) do |url|
          unless url.strip.empty?
            url = "file://#{url}" unless url[%r{\A\w+:/}]
            bookmark_url(wmii, url)
          end
        end
      end,
      "bookmark-delete" => lambda do |wmii|
        bm_manager = BookmarkManager.new(BOOKMARK_FILE)
        delete_bookmark = lambda do |bm|
          if bm_manager.remove_bookmark bm.url
            bm_manager.transaction{ |bmanager| bmanager.save! }
            LOGGER.info "Delete bookmark #{bm.description.inspect} -> #{bm.url}."
          else
            LOGGER.info "Could not delete bookmark #{bm.description.inspect} -> #{bm.url}."
          end
        end
        refine_choices = lambda do |bookmarks|
          options = bookmarks.sort_by{|x| x.description}.map do |x| 
            "#{x.description} : #{x.url}"
          end
          wmii.wmiimenu(options) do |condition|
            condition = condition.strip
            unless condition.empty?
              if condition == "!o"
                if bookmarks.size <= 
                    (limit = wmii.plugin_config["standard:bookmark"]["multiple-delete-limit"])
                  bookmarks.each do |bm|
                    delete_bookmark.call(bm)
                  end
                else
                  LOGGER.error "Tried to delete #{bookmarks.size} bookmarks at a time."
                  LOGGER.error "Refusing since it's over multiple-delete-limit (#{limit})."
                end
              elsif bm = bm_manager[condition[/ : (\S+)$/,1]]
                delete_bookmark.call(bm)
              else
                choices = bm_manager.refine_selection(condition, bookmarks)
                refine_choices.call(choices) unless choices.empty?
              end
            end
          end
        end
        refine_choices.call(bm_manager.bookmarks)
      end,
      "bookmark-edit" => lambda do |wmii|
        bm_manager = BookmarkManager.new(BOOKMARK_FILE)
        refine_choices = lambda do |bookmarks|
          options = bookmarks.sort_by{|x| x.description}.map do |x| 
            "#{x.description} : #{x.url}"
          end
          wmii.wmiimenu(options) do |condition|
            condition = condition.strip
            unless condition.empty?
              if bm = bm_manager[condition[/ : (\S+)$/,1]]
                title_variants = [bm.description]
                title_variants <<
                  (bm.description + ' ' + bm.tags.join(' ')) unless bm.tags.empty?
                wmii.wmiimenu(title_variants) do |choice|
                  tags = choice[/\s+(:\S+\s*)+$/] || ""
                  description = choice[0..-1-tags.size].strip
                  if description =~ /\S/
                    LOGGER.info "Edited bookmark #{description.inspect} -> #{bm.url}."
                    bm_manager.add_bookmark(description, bm.url, tags.strip.split(/\s+/), Time.new)
                    bm_manager.transaction{ |bmanager| bmanager.save! }
                  end
                end
              else
                choices = bm_manager.refine_selection(condition, bookmarks)
                refine_choices.call(choices) unless choices.empty?
              end
            end
          end
        end
        refine_choices.call(bm_manager.bookmarks)
      end
    })
  end

  def_settings("bookmark/multiple-open") do |wmii|
    wmii.plugin_config["standard:bookmark"]["multiple-open-limit"] = 10
  end

  def_settings("bookmark/multiple-delete") do |wmii|
    wmii.plugin_config["standard:bookmark"]["multiple-delete-limit"] = 30
  end

  def_settings("bookmark/del.icio.us importer") do |wmii|
    wmii.plugin_config["standard:bookmark"]["refresh_period"] = 30
    Thread.new do
      sleep 20  # time to get the wmiirc-config.rb loaded
      if wmii.plugin_config["standard:bookmark"]["del.icio.us-user"] and
         wmii.plugin_config["standard:bookmark"]["del.icio.us-password"]

        mode = wmii.plugin_config["standard:bookmark"]["del.icio.us-mode"] || :unidirectional
        last_update_time = nil
        loop do
          begin
            sync_delicious_bookmarks(wmii, mode, last_update_time)
            last_update_time = Time.now
          rescue Exception
            LOGGER.error "Error while sync'ing bookmarks."
            LOGGER.error $!.exception
            puts $!.backtrace
          end
          sleep(60 * wmii.plugin_config["standard:bookmark"]["refresh_period"])
        end
      end
    end
  end

  standard_bookmark_protocols = {
    'http' => {
      :open_urls => lambda do |wmii,bms|
        browser = ENV["BROWSER"] || '/etc/alternatives/x-www-browser'
        urls = bms.map{|bm| bm[:uri].to_s}.join "' '"
        system "wmiisetsid #{browser} '#{urls}' &"
      end,
      :get_title => lambda do |wmii,uri|
        resolved_uri = uri.clone
        resolved_uri.host = Resolv.new.getaddress resolved_uri.host
        contents = open(resolved_uri.to_s, "Host" => uri.host, "User-Agent" => BOOKMARK_AGENT){|f| f.read}
        title = CGI.unescapeHTML((contents[%r{title>(.*)</title>}im, 1] || "").strip).gsub(/&[^;]+;/, "")
        title.gsub!(/\s+/, " ")
        [title, title.downcase, title.capitalize]
      end
    },
    'ssh' => {
      :open_urls => lambda do |wmii,bms|
        term = wmii.plugin_config["standard"]["x-terminal-emulator"] || "xterm"
        bms.each do |bm|
          uri = bm[:uri]
          ssh_host = uri.host
          ssh_host = "#{uri.user}@" + ssh_host unless uri.user.nil?
          ssh_port = "-p #{uri.port}" unless uri.port.nil?
          system "wmiisetsid #{term} -T '#{bm[:bm].url}' -e 'ssh #{ssh_host} #{ssh_port} || read' &"
        end
      end,
      :get_title => lambda do |wmii,uri|
        title = uri.host
        title = "#{uri.user}@" + title unless uri.user.nil?
        title << ":#{uri.port.to_s}" unless uri.port.nil?
        title
      end
    }
  }
  standard_bookmark_protocols['https'] = standard_bookmark_protocols['http']
  standard_bookmark_protocols['ftp'] =
    {
      :open_urls => standard_bookmark_protocols['http'][:open_urls],
      :get_title => standard_bookmark_protocols['ssh'][:get_title]
    }
  

  def_settings("bookmark/protocols") do |wmii|
    wmii.plugin_config["standard:bookmark"]["protocols"] ||= {}
    wmii.plugin_config["standard:bookmark"]["protocols"] =
      standard_bookmark_protocols.merge wmii.plugin_config["standard:bookmark"]["protocols"]
  end

  binding("bookmark", "MODKEY-Shift-b") do |wmii,|
    Thread.new do
      url = `wmiipsel`.strip
      url = "file://#{url}" unless url[%r{\A\w+:/}]
      bookmark_url(wmii, url)
    end
  end

  binding("bookmark-open", "MODKEY-b") do |wmii,|
    bm_manager = BookmarkManager.new(BOOKMARK_FILE)
    open_bookmark = lambda do |bms|
      bookmark_protocols = wmii.plugin_config["standard:bookmark"]["protocols"]
      bm_hash = {}
      bms.each do |bm|
        LOGGER.debug "Opening bookmark #{bm.description.inspect} -> #{bm.url}."
        begin
          uri = URI.parse bm.url
        rescue
          LOGGER.error "Failed to open #{bm.url}: invalid URI. Corrupted bookmarks.txt?"
          return
        end
        protocol_desc = bookmark_protocols[uri.scheme]
        unless protocol_desc.nil?
          bm_hash[protocol_desc] = (bm_hash[protocol_desc].to_a << {:bm => bm, :uri => uri})
        else
          LOGGER.error "Failed to open #{bm.url}: unknown protocol #{uri.scheme.inspect}."
        end
      end
      bm_hash.each do |proto,bms|
        proto[:open_urls].call(wmii,bms)
      end
    end
    refine_choices = lambda do |bookmarks|
      options = bookmarks.sort_by{|x| x.description}.map do |x| 
        "#{x.description} : #{x.url}"
      end
      wmii.wmiimenu(options) do |condition|
        condition = condition.strip
        unless condition.empty?
          if condition == "!o"
            if bookmarks.size <= 
               (limit = wmii.plugin_config["standard:bookmark"]["multiple-open-limit"])
              bookmarks.each do |bm|
                bm.date = Time.new
              end
              bm_manager.transaction{|bm| bm.save!}
              open_bookmark.call(bookmarks)
            else
              LOGGER.error "Tried to open #{bookmarks.size} bookmarks at a time."
              LOGGER.error "Refusing since it's over multiple-open-limit (#{limit})."
            end
          elsif bm = bm_manager[condition[/ : (\S+)$/,1]]
            bm.date = Time.new
            bm_manager.transaction{ bm_manager.save! }
            open_bookmark.call([bm])
          else
            choices = bm_manager.refine_selection(condition, bookmarks)
            refine_choices.call(choices) unless choices.empty?
          end
        end
      end
    end
    refine_choices.call(bm_manager.bookmarks)
  end
end

