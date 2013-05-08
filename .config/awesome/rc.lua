-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
-- Using Vicious 
	-- cd .config/awesome
	-- git clone http://git.sysphere.org/vicious
vicious = require("vicious")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/theme/theme.lua")

-- Private naughty config
naughty.config.default_preset.timeout          = 8
naughty.config.default_preset.screen           = 1
naughty.config.default_preset.font             = "sans 12"
naughty.config.default_preset.position         = "top_left"
naughty.config.default_preset.margin           = 13
naughty.config.default_preset.ontop            = true
naughty.config.default_preset.fg = beautiful.fg_focus
naughty.config.default_preset.bg = beautiful.bg_focus
naughty.config.presets.normal.border_color     = '#cc6699'
naughty.config.default_preset.border_width     = 1

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.add_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- This is used later as the default terminal and editor to run.
terminal = "xterm"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
  awful.layout.suit.tile,
  awful.layout.suit.tile.left,
  awful.layout.suit.tile.bottom,
  awful.layout.suit.tile.top,
  awful.layout.suit.fair,
  awful.layout.suit.fair.horizontal,
--  awful.layout.suit.spiral,
--  awful.layout.suit.spiral.dwindle,
  awful.layout.suit.max,
--  awful.layout.suit.max.fullscreen,
--  awful.layout.suit.magnifier
  awful.layout.suit.floating,
}
-- }}}

-- {{{ Tags
tags = {
	names = { " Firefox ", " Mail_IRC ", " Terminals ", " Vim "," Misc ","","" },
}
for s = 1, screen.count() do
	tags[s] = awful.tag(tags.names, s, layouts[5])
	end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu

my_term_menu = {
	{ "Xterm", terminal },
	{ "Screen", terminal.." -e screen" },
}
my_fm_menu = {
	{ "Ranger", terminal.." -e ranger" },
	{ "Pcmanfm", "pcmanfm" },
}
my_media_menu = {
	{ "Audacious", "audacious"},
	{ "Alsamixer",terminal.." -e alsamixer" },
}
mymainmenu = awful.menu({ items = { 
	{ "Terminal", my_term_menu },
	{ "File", my_fm_menu },
	{ "Media", my_media_menu },
	{ "Firefox", "firefox"},
	{ "Gqview", "gqview"},
	{ "Freemind", "freemind"},
}
})

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Initialize widget
-- vicious.register(widget, wtype, format, interval, warg)

-- Battery
batwidget = widget({ type = 'textbox', })
vicious.register(batwidget, vicious.widgets.bat, "$2 $1", 10, "BAT0")

-- Volume
volwidget = widget({ type = 'textbox'})
vicious.register(volwidget, vicious.widgets.volume, "$1$2", 1, "Master")

-- Uptime
uptimewidget = widget({ type = 'textbox', })
vicious.register(uptimewidget, vicious.widgets.uptime, "load: $4 | up: $1d, $2:$3", 2)

-- Memory usage
memwidget = widget({ type = "textbox" })
vicious.register(memwidget, vicious.widgets.mem, "mem: $1%", 2)

-- Cpu usage
cpuwidget = widget({ type = "textbox" })
vicious.register(cpuwidget, vicious.widgets.cpu, "cpu: $1%", 2)

-- {{{ CPU temperature
local thermalwidget = widget({ type = "textbox" })
vicious.register(thermalwidget, vicious.widgets.thermal, " $1°C", 2, { "coretemp.0", "core"} )
-- }}}

-- Network
netwidget = widget({ type = 'textbox' })
function get_netspeed()
    local netspeed = {}
    local deviceinfo = vicious.widgets.net()
    for device in io.popen("ip route | awk '/default/ {print $5}' "):lines() do
        netspeed['{down_kb}'] = (netspeed['{down_kb}'] or 0) + deviceinfo['{' .. device .. " " .. "down_kb" .. '}']
        netspeed['{up_kb}'] = (netspeed['{up_kb}'] or 0) + deviceinfo['{' .. device .. " " .. "up_kb" .. '}']
	netspeed['{device}'] = device
    end
    return netspeed
end
vicious.register(netwidget, get_netspeed, "${device}: ${down_kb} kb↓  ${up_kb} kb↑", 1)

-- }}}


-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" })
require('calendar2')
calendar2.addCalendarToWidget(mytextclock, "<span color='green'>%s</span>")

-- Create a systray
mysystray = widget({ type = "systray" })

-- Private decoration
myicon = widget({ type = "imagebox" })
myicon.image = image(beautiful.awesome_icon)
myspace = widget({ type = "textbox" })
myspace.text = " "
myseperator= widget({ type = "textbox" })
myseperator.text = " | "

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if not c:isvisible() then
                                                  awful.tag.viewonly(c:tags()[1])
                                              end
                                              client.focus = c
                                              c:raise()
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", height = 25,  screen = s })

    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
  	    myspace,myspace,
	    mylayoutbox[s],
	    myspace,myspace,
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
	batwidget,
	myseperator,volwidget,myspace,
        s == 1 and mysystray or nil,
	-- Private widgets
	myseperator,mytextclock, 
	myseperator,uptimewidget,
	myseperator,thermalwidget,cpuwidget,
	myseperator,memwidget,
	-- myseperator,netwidget,
	myseperator,myspace,myspace,myspace,myspace,
	
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings

globalkeys = awful.util.table.join(
-- {{{ Private global key bindings
-- modkey: Mod4(win)
-- mod1: alt
    -- volume keys
    awful.key({ }, "XF86AudioLowerVolume", function () awful.util.spawn("amixer -q sset Master 5%- unmute") end),
    awful.key({ }, "XF86AudioRaiseVolume", function () awful.util.spawn("amixer -q sset Master 5%+ unmute") end),
    awful.key({ }, "XF86AudioMute", function () awful.util.spawn("amixer -q sset Master mute") end),
    awful.key({ modkey }, "Up", function () awful.util.spawn("amixer -q sset Master 5%+ unmute") end),
    awful.key({ modkey }, "Down", function () awful.util.spawn("amixer -q sset Master 5%- unmute") end),
    
    -- mpc control
    awful.key({}, "XF86AudioPlay", function () awful.util.spawn("mpc toggle") end),
    awful.key({}, "XF86AudioStop", function () awful.util.spawn("mpc stop") end),
    awful.key({}, "XF86AudioPrev", function () awful.util.spawn("mpc prev") end),
    awful.key({}, "XF86AudioNext", function () awful.util.spawn("mpc next") end),
    awful.key({ "Mod1" }, "Down", function () awful.util.spawn("mpc toggle") end),
    awful.key({ "Mod1" }, "Up", function () awful.util.spawn("mpc stop") end),
    awful.key({ "Mod1" }, "Left", function () awful.util.spawn("mpc prev") end),
    awful.key({ "Mod1" }, "Right", function () awful.util.spawn("mpc next") end),

    awful.key({ "Mod1" }, "i", function()
	    local song = awful.util.pread("mpc status"):sub(1,-2)
	    if song:len() == 0 then
		    song = "- nothing playing -"
	    end
	    naughty.notify({ text = song, title = "Currently playing:", timeout = 5 })
    end),

    -- alt + contrl+l: lock screen
    awful.key({"Mod1", "Control" }, "l", function () awful.util.spawn_with_shell("xset dpms 0 0 5 ; slock ; xset dpms 0 0 0") end),
    
    -- modkey + h/l: tag previous/next 
    awful.key({ modkey,     }, "h",  awful.tag.viewprev       ),
    awful.key({ modkey,     }, "l",  awful.tag.viewnext       ),

    -- modkey + `: history window
    awful.key({ modkey,           }, "`", awful.tag.history.restore),

    -- modkey + Print: print full screen
    awful.key({ }, "Print",
    function ()
    	awful.util.spawn("scrot -e 'mv $f ~/Pictures/Shot/'")
    	os.execute("sleep 0.5")
    	naughty.notify({ title="Screenshot", text="The full screen captured" })
    end),
    
    -- alt + Print: print focused window
    awful.key({ "Mod1" }, "Print",
    function ()
    	os.execute("sleep 0.5")
    	awful.util.spawn("scrot -s -e 'mv $f ~/Pictures/Shot/'")
    	os.execute("sleep 0.5")
    	naughty.notify({ title="Screenshot", text="The selected window captured" })
    end),
    
    -- mod + e :start ranger
    awful.key({ modkey }, "e", function () awful.util.spawn("xterm -e ranger") end),
    
    -- mod + w :start pcmanfm
    awful.key({ modkey }, "w", function () awful.util.spawn("pcmanfm") end),
    
    -- mod + s :xterm with screen
    awful.key({ modkey }, "s", function () awful.util.spawn("xterm -e screen") end),
    
    -- mod + d : stardict, shift + mod + d : type a word then querry
    awful.key({ modkey }, "d", function ()

    	local f = io.popen("xsel -o")
    	local new_word = f:read("*a")
    	f:close()
    
    	if frame ~= nil then
    		naughty.destroy(frame)
    		frame = nil
    		if old_word == new_word then
    			return
    		end
    	end
    	old_word = new_word
    
    	local fc = ""
    	--local f  = io.popen("sdcv -n --utf8-output -u '21世紀英漢漢英雙向詞典' "..new_word)
    	--not use a only dict
    	local f  = io.popen("sdcv -n --utf8-output "..new_word)
    	for line in f:lines() do
    		fc = fc .. line .. '\n'
    	end
    	f:close()
    	frame = naughty.notify({
		text = fc,
		position = "top_left",
		timeout = 10,
		fg = "#000000",
		bg = "#FFFFCC",
		border_color = 0
	})
    end),
    awful.key({ modkey, "Shift" }, "d", function ()
    	awful.prompt.run({prompt = "Dict: "}, mypromptbox[mouse.screen].widget, function(cin_word)
    		naughty.destroy(frame)
    		if cin_word == "" then
    			return
    		end
    
    		local fc = ""
    		--local f  = io.popen("sdcv -n --utf8-output -u '21世紀英漢漢英雙向詞典' "..cin_word)
    		local f  = io.popen("sdcv -n --utf8-output "..cin_word)
    		for line in f:lines() do
    			fc = fc .. line .. '\n'
    		end
    		f:close()
		frame = naughty.notify({
			text = fc,
			position = "top_left",
			timeout = 10,
			fg = "#000000",
			bg = "#FFFFCC",
			border_color = 0
		})
    	end, nil, awful.util.getdir("cache").."/dict")
    end),
-- }}}
    
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),
    
    awful.key({ modkey,           }, "j",
    function ()
    	awful.client.focus.byidx( 1)
    	if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey,           }, "k",
    function ()
    	awful.client.focus.byidx(-1)
    	if client.focus then client.focus:raise() end
    end),
    
    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
    function ()
    	awful.client.focus.history.previous()
    	if client.focus then
    		client.focus:raise()
    	end
    end),
    
    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),
    
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),
    
    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),
    
    awful.key({ modkey }, "x",
    function ()
    	awful.prompt.run({ prompt = "Run Lua code: " },
    	mypromptbox[mouse.screen].widget,
    	awful.util.eval, nil,
    	awful.util.getdir("cache") .. "/history_eval")
    end)
)

clientkeys = awful.util.table.join(
    -- {{{ Private client key bindings
    awful.key({ "Mod1" }, "F4", function (c) c:kill() end),

    -- }}}
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",      function (c) c.minimized = not c.minimized    end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    -- Private rules
    { rule = { class = "MPlayer" },},
      properties = { floating = true },
    { rule = { class = "Flashplayer" },
      properties = { floating = true }, },
    { rule = { class = "feh" },
      properties = { floating = true } },
    { rule = { class = "Skype" },
      properties = { floating = true } },
    { rule = { class = "Gimp" },
      properties = { floating = true } },
    { rule = { class = "Plugin-container"},
      properties = { floating = true } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
