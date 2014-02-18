-- Standard awesome library
local gears = require("gears")
awful = require("awful")
awful.autofocus = require("awful.autofocus")
awful.rules = require("awful.rules")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
beautiful = require("beautiful")
-- Notification library
naughty = require("naughty")
menubar = require("menubar")
-- Using Vicious
	-- cd .config/awesome
	-- git clone http://git.sysphere.org/vicious
vicious = require("vicious")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/theme/theme.lua")

-- Private naughty config
naughty.config.defaults.timeout          = 8
naughty.config.defaults.screen           = 1
naughty.config.defaults.font             = "sans 12"
naughty.config.defaults.position         = "top_left"
naughty.config.defaults.margin           = 13
naughty.config.defaults.ontop            = true
naughty.config.defaults.fg = beautiful.fg_focus
naughty.config.defaults.bg = beautiful.bg_focus
naughty.config.defaults.border_color     = '#cc6699'
naughty.config.defaults.border_width     = 1

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
    awesome.connect_signal("debug::error", function (err)
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
terminal = "urxvt"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
  awful.layout.suit.tile,
  awful.layout.suit.tile.left,
  awful.layout.suit.tile.bottom,
--  awful.layout.suit.tile.top,
  awful.layout.suit.fair,
  awful.layout.suit.fair.horizontal,
--  awful.layout.suit.spiral,
--  awful.layout.suit.spiral.dwindle,
  awful.layout.suit.max,
--  awful.layout.suit.max.fullscreen,
  awful.layout.suit.magnifier,
  awful.layout.suit.floating
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
tags = {
	names = { " 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "},
}
for s = 1, screen.count() do
	tags[s] = awful.tag(tags.names, s, layouts[1])
	end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu

mymainmenu = awful.menu({ items = {
	{ "URxvt", terminal },
	{ "Screen", terminal.." -e screen" },
	{ "Ranger", terminal.." -e ranger" },
	{ "Pcmanfm", "pcmanfm" },
	{ "Audacious", "audacious"},
	{ "Alsamixer",terminal.." -e alsamixer" },
	{ "Firefox", "firefox"},
	{ "Gqview", "gqview"},
	{ "Freemind", "freemind"},
	{ "ARandR", "arandr"},

}
})

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })
-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ private Vicious

-- Battery
batwidget = wibox.widget.textbox()
vicious.register(batwidget, vicious.widgets.bat, "$2 $1", 10, "BAT0")

-- Volume
volwidget = wibox.widget.textbox()
vicious.register(volwidget, vicious.widgets.volume, "$1$2", 1, "Master")

-- Uptime
uptimewidget = wibox.widget.textbox()
vicious.register(uptimewidget, vicious.widgets.uptime, "load: $4 | up: $1d, $2:$3", 2)

-- Memory usage
memwidget = wibox.widget.textbox()
vicious.register(memwidget, vicious.widgets.mem, "mem: $1%", 2)

-- Cpu usage
cpuwidget = wibox.widget.textbox()
vicious.register(cpuwidget, vicious.widgets.cpu, "cpu: $1%", 2)

-- CPU temperature
local thermalwidget = wibox.widget.textbox()
vicious.register(thermalwidget, vicious.widgets.thermal, " $1°C", 2, { "coretemp.0", "core"} )
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock(" %a %b %d, %H:%M ", 1)

calendar2 = require('calendar2')
calendar2.addCalendarToWidget(mytextclock, "<span color='green'>%s</span>")

-- Private decoration
myicon = wibox.widget.imagebox()
myicon:set_image(beautiful.awesome_icon)
myspace = wibox.widget.textbox()
myspace:set_text(" ")
myseperator =  wibox.widget.textbox()
myseperator:set_text(" | ")

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
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                               if c == client.focus then
                                                   c.minimized = true
                                               else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  client.focus = c
                                                  c:raise()
					      end
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
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))

    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)
    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", height = 25,  screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(myspace)
    left_layout:add(mylayoutbox[s])
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(myspace)
    right_layout:add(myspace)
    right_layout:add(myspace)
    right_layout:add(memwidget)
    right_layout:add(myseperator)
    right_layout:add(cpuwidget)
    right_layout:add(thermalwidget)
    right_layout:add(myseperator)
    right_layout:add(uptimewidget)
    right_layout:add(myseperator)
    right_layout:add(mytextclock)
    right_layout:add(myseperator)
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(myspace)
    right_layout:add(volwidget)
    right_layout:add(myspace)
    right_layout:add(batwidget)
    right_layout:add(myspace)

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
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
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),

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
    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    -- Private global key bindings
    awful.key({ modkey }, "s", function () awful.util.spawn_with_shell("xset dpms 0 0 5 ; slock ; xset dpms 0 0 0") end),
    awful.key({ modkey, "Shift" }, "s", function () awful.util.spawn_with_shell("sudo /usr/sbin/pm-suspend& slock") end),
    awful.key({ modkey }, "e", function () awful.util.spawn(terminal.." -e ranger") end),
    awful.key({ "Mod1" }, "n", naughty.toggle),

    awful.key({ modkey,           }, "Escape", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey,           }, "q", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey,           }, "`", awful.tag.history.restore),
    awful.key({ modkey }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    awful.key({ }, "Print",
    function ()
        awful.util.spawn("scrot -e 'mv $f ~/Pictures/Shot/'")
        os.execute("sleep 0.5")
        naughty.notify({ title="Screenshot", text="The full screen captured" })
    end),
    awful.key({ "Mod1" }, "Print",
    function ()
        os.execute("sleep 0.5")
        awful.util.spawn("scrot -s -e 'mv $f ~/Pictures/Shot/'")
    end),


    -- mod + d : stardict
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

    awful.key({ }, "XF86AudioLowerVolume", function () awful.util.spawn("amixer -q sset Master 5%- unmute") end),
    awful.key({ }, "XF86AudioRaiseVolume", function () awful.util.spawn("amixer -q sset Master 5%+ unmute") end),
    awful.key({ }, "XF86AudioMute", function () awful.util.spawn("amixer -q sset Master mute") end),
    awful.key({ modkey }, "Up", function () awful.util.spawn("amixer -q sset Master 5%+ unmute") end),
    awful.key({ modkey }, "Down", function () awful.util.spawn("amixer -q sset Master 5%- unmute") end)

)

clientkeys = awful.util.table.join(
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
        end),

    -- Private client key bindings
    awful.key({ "Mod1" }, "F4", function (c) c:kill() end)
)


-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
			local tag = awful.tag.gettags(screen)[i]
			if tag then
			   awful.tag.viewonly(tag)
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
		      local tag = awful.tag.gettags(screen)[i]
		      if tag then
		         awful.tag.viewtoggle(tag)
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
		      local tag = awful.tag.gettags(client.focus.screen)[i]
		      if client.focus and tag then
		          awful.client.movetotag(tag)
		      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      local tag = awful.tag.gettags(client.focus.screen)[i]
                      if client.focus and tag then
                          awful.client.toggletag(tag)
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
                     focus = awful.client.focus.filter,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    -- Private rules
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "Flashplayer" },
      properties = { floating = true } },
    { rule = { class = "Plugin-container"},
      properties = { floating = true } },
    { rule = { class = "feh" },
      properties = { floating = true } },
    { rule = { class = "Arandr" },
      properties = { floating = true } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
     -- Enable sloppy focus
     c:connect_signal("mouse::enter", function(c)
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

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
 end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
