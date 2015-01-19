# Based on tin.py:
# https://github.com/qtile/qtile-examples/blob/master/tin.py

try:
    from libqtile.manager import Key, Group
except ImportError:
    from libqtile.config import Key, Group

from libqtile.manager import Click, Drag, Screen
from libqtile.manager import Screen, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
import os

sup = "mod4"
alt = "mod1"

keys = [
    Key(
        [alt], "Tab",
        lazy.layout.down()
    ),
    Key(
        [alt, "shift"], "Tab",
        lazy.layout.up()
    ),
    Key(
        [sup, "control"], "k",
        lazy.layout.shuffle_down()
    ),
    Key(
        [sup, "control"], "j",
        lazy.layout.shuffle_up()
    ),
    Key(
        [sup], "space",
        lazy.layout.next()
    ),
    Key(
        [sup, "shift"], "space",
        lazy.layout.rotate()
    ),
    Key(
        [sup, "shift"], "Return",
        lazy.layout.toggle_split()
    ),
    Key([sup], "Return", lazy.spawn("urxvtc")),
    Key([sup], "f", lazy.spawn("firefox")),
    Key([sup], "z", lazy.spawn("spacefm")),
    Key([sup, 'control'], "l", lazy.spawn("xtrlock")),
    Key([sup], "p", lazy.spawn("pidgin")),
    Key([sup], "s", lazy.spawn("subl")),
    Key([sup], 'r', lazy.spawncmd()),
    Key([sup], 'w', lazy.window.kill()),
    Key([sup, 'control'], 'r', lazy.restart()),
    Key([sup, 'control'], 'q', lazy.shutdown()),
    Key(
        [], "XF86AudioRaiseVolume",
        lazy.spawn("amixer -c 0 -q set Master 2dB+")
    ),
    Key(
        [], "XF86AudioLowerVolume",
        lazy.spawn("amixer -c 0 -q set Master 2dB-")
    ),

    # cycle to previous group
    Key([sup], "Left", lazy.group.prevgroup()),
    # cycle to next group
    Key([sup], "Right", lazy.group.nextgroup()),
    # windows style alt-tab/alt-shift-tab
    Key([sup], "Tab", lazy.nextlayout()),
    Key([sup, "shift"], "Tab", lazy.previouslayout()),
    # PRINT SCREEN
    Key([sup], "F10", lazy.spawn(
        "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f  ~/Pictures'")),

    Key([alt], "t", lazy.window.toggle_floating()),
]

groups = [
    Group("1"),
    Group("2"),
    Group("3"),
    Group("4"),
    Group("5"),
    Group("6"),
    Group("7"),
    Group("8"),
]
for i in groups:
    keys.append(
        Key([sup], i.name, lazy.group[i.name].toscreen())
    )
    keys.append(
        Key([sup, "shift"], i.name, lazy.window.togroup(i.name))
    )

# This allows you to drag windows around with the mouse if you want.
mouse = [
    Drag([sup], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([sup], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([sup], "Button2", lazy.window.bring_to_front())
]


layouts = (
    layout.Tile(ratio=0.5),
    layout.Max(),
    layout.RatioTile(),
    layout.Matrix(),
    layout.MonadTall(),
    layout.Stack(),
    layout.Zoomy(),
)

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(fontsize=10),
                widget.Sep(),
                widget.CurrentLayout(fontsize=10),
                widget.Sep(),
                widget.WindowName(fontsize=12),
                widget.Sep(),
                widget.Prompt(fontsize=12),
                widget.CPUGraph(frequency=1, samples=50, line_width=1, width=50,
                                graph_color='FF2020',
                                fill_color='C01010'),
                widget.MemoryGraph(frequency=1, samples=50, line_width=1, width=50,
                                   graph_color='0066FF',
                                   fill_color='001188'),
                widget.NetGraph(frequency=1, samples=50, line_width=1,
                                width=50, interface="eth0",
                                graph_color='22FF44',
                                fill_color='11AA11'),
                widget.Sep(),
                widget.YahooWeather(
                    location='Rio de Janeiro, RJ', update_interval=10, fontsize=10),
                widget.Systray(),
                widget.Sep(),
                widget.Clock('%d-%m-%Y %a %I:%M %p', fontsize=12),
            ],
            25,
        ),
    ),
]

def show_shortcuts():
    key_map = {"mod1": "alt", "mod4": "super"}
    shortcuts_path = "{0}/{1}".format(os.environ["HOME"], "qtile_shortcuts")
    shortcuts = open("{0}".format(shortcuts_path), 'w')
    shortcuts.write("{0:30}| {1:50}\n".format("KEYS COMBINATION", "COMMAND"))
    shortcuts.write("{0:80}\n".format("=" * 80))
    for key in keys:
        key_comb = ""
        for modifier in key.modifiers:
            key_comb += key_map.get(modifier, modifier) + "+"
        key_comb += key.key
        shortcuts.write("{0:30}| ".format(key_comb))
        cmd_str = ""
        for command in key.commands:
            cmd_str += command.name + " "
            for arg in command.args:
                cmd_str += "{0} ".format(repr(arg))
        shortcuts.write("{0:50}\n".format(cmd_str))
        shortcuts.write("{0:80}\n".format("-" * 80))
    shortcuts.close()
    return lazy.spawn("xterm -wf -e less {0}".format(shortcuts_path))


@hook.subscribe.client_new
def dialogs(window):
    if(window.window.get_wm_type() == 'dialog'
            or window.window.get_wm_transient_for()):
        window.floating = True

# More shortcut
keys.append(Key([sup], "h", show_shortcuts()))

main = None
follow_mouse_focus = True
cursor_warp = False
floating_layout = layout.Floating()
mouse = ()
