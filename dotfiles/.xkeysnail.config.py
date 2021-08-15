# -*- coding: utf-8 -*-

import re
from xkeysnail.transform import *

# * [Global modemap] Change modifier keys as in xmodmap
define_modmap({Key.CAPSLOCK: Key.LEFT_CTRL,
               # Poker键盘，交换Escape和grave
               # Key.ESC: Key.GRAVE,
               # Key.GRAVE: Key.ESC
               })

# * [Conditional modmap] Change modifier keys in certain applications
# define_conditional_modmap(re.compile(r'Emacs'), {
#     Key.RIGHT_CTRL: Key.ESC,
# })

# * [Multipurpose modmap] Give a key two meanings.
# A normal key when pressed and released, and a modifier key when held down with another key. See Xcape, Carabiner and caps2esc for ideas and concept.
# define_multipurpose_modmap(
#     # Enter is enter when pressed and released. Control when held down.
#     # {Key.ENTER: [Key.ENTER, Key.RIGHT_CTRL]}

#     # Capslock is escape when pressed and released. Control when held down.
#     {Key.CAPSLOCK: [Key.ESC, Key.LEFT_CTRL]}
#     # To use this example, you can't remap capslock with define_modmap.
# )

# * Keybindings for Firefox/Chromium
firefox_dict = {
    # Ctrl+Alt+j/k to switch next/previous tab
    K("M-n"): K("C-TAB"),
    K("M-p"): K("C-Shift-TAB"),
    # Type C-j to focus to the content
    K("C-i"): K("C-f6")
    # very naive "Edit in editor" feature (just an example)
    # K("C-o"): [K("C-a"), K("C-c"), launch(["gedit"]), sleep(0.5), K("C-v")]
    }
define_keymap(
    lambda wm_class: wm_class and wm_class[-1] in ("Firefox", "Chromium"),
    firefox_dict,
    "Firefox and Chromium")

# * Keybindings for Qpdfview
define_keymap(
    lambda wm_class: wm_class and wm_class[-1] in ("qpdfview"),
    {
        K("q"): [K("q"), K("C-f"), K("esc")],
        K("C-apostrophe"): [K("M-Shift-m"), K("M-t")],
        K("C-j"): [K("enter"), K("M-Shift-m")],
    },
    "Qpdfview")

# * Keybindings for Zeal https://github.com/zealdocs/zeal/
# define_keymap(re.compile("Zeal"), {
#     # Ctrl+s to focus search area
#     K("C-s"): K("C-k"),
# }, "Zeal")

# * Emacs-like keybindings in non-Emacs applications
emacs_dict = {
    # Cursor
    K("C-b"): with_mark(K("left")),
    K("C-f"): with_mark(K("right")),
    K("C-p"): with_mark(K("up")),
    K("C-n"): with_mark(K("down")),
    K("C-h"): with_mark(K("backspace")),
    # Forward/Backward word
    K("M-b"): with_mark(K("C-left")),
    K("M-f"): with_mark(K("C-right")),
    # Beginning/End of line
    K("C-a"): with_mark(K("home")),
    K("C-e"): with_mark(K("end")),
    # Page up/down
    K("M-v"): with_mark(K("page_up")),
    K("C-v"): with_mark(K("page_down")),
    # Beginning/End of file
    K("M-Shift-comma"): with_mark(K("C-home")),
    K("M-Shift-dot"): with_mark(K("C-end")),
    # Newline
    K("C-m"): K("enter"),
    K("C-j"): K("enter"),
    K("C-o"): [K("enter"), K("left")],
    # Copy
    K("C-w"): [K("C-x"), set_mark(False)],
    K("M-w"): [K("C-c"), set_mark(False)],
    K("C-y"): [K("C-v"), set_mark(False)],
    # Delete
    K("C-d"): [K("delete"), set_mark(False)],
    K("M-d"): [K("C-delete"), set_mark(False)],
    # Kill line
    K("C-k"): [K("Shift-end"), K("C-x"),
               set_mark(False)],
    # Undo
    K("C-slash"): [K("C-z"), set_mark(False)],
    K("C-M-slash"): [K("C-y"), set_mark(False)],
    K("C-Shift-ro"): K("C-z"),
    # Mark
    K("C-semicolon"): set_mark(True),
    K("C-M-semicolon"): with_or_set_mark(K("C-right")),
    # Search
    K("C-s"): K("F3"),
    K("C-r"): K("Shift-F3"),
    K("M-Shift-key_5"): K("C-h"),
    # Cancel
    K("C-g"): [K("esc"), set_mark(False)],
    # Escape
    K("C-q"): escape_next_key,
    # Menu
    K("Super-Shift-o"): K("Compose"),
    # C-x YYY
    K("C-x"): {
        # C-x h (select all)
        K("h"): [K("C-home"), K("C-a"),
                 set_mark(True)],
        # C-x C-f (open)
        K("C-f"): K("C-o"),
        # C-x C-s (save)
        K("C-s"): K("C-s"),
        # C-x k (kill tab)
        K("k"): K("C-f4"),
        # C-x C-c (exit)
        K("C-c"): K("C-q"),
        # cancel
        K("C-g"): pass_through_key,
        # C-x u (undo)
        K("u"): [K("C-z"), set_mark(False)],
    }}
define_keymap(
    lambda wm_class: wm_class and wm_class[-1] not in ("Emacs", "URxvt", "Vncviewer", "Blender", "scrcpy", "VirtualBox Machine", "org.remmina.Remmina") and wm_class[1] not in ("winword.exe"),
    emacs_dict,
    "Emacs-like keys")

# * Remove Alt tip with win in Office/Wps
# define_conditional_multipurpose_modmap(
#     lambda wm_class: wm_class and wm_class[1] in ("winword.exe", "excel.exe", "powerpnt.exe", "wps", "et", "wpp"),
#     {
#         Key.LEFT_ALT: [Key.LEFT_META, Key.LEFT_ALT],
#     })
# dict_disable_alt = {
#     K("M-b"): [with_mark(K("C-left"))],
#     K("M-f"): [with_mark(K("C-right"))],
#     K("M-v"): [with_mark(K("page_up"))],
#     K("M-Shift-comma"): [with_mark(K("C-home"))],
#     K("M-Shift-dot"): [with_mark(K("C-end"))],
#     K("M-w"): [K("C-c"), set_mark(False)],
#     K("M-d"): [K("C-delete"), set_mark(False)],
#     K("C-M-slash"): [K("C-y"), set_mark(False)],
#     K("C-M-semicolon"): [with_or_set_mark(K("C-right"))],
#     K("M-Shift-key_5"): [K("C-h")]}
# emacs_dict_disable_alt = emacs_dict.copy()
# emacs_dict_disable_alt.update(dict_disable_alt)
# define_keymap(
#     lambda wm_class: wm_class and wm_class[1] in ("winword.exe", "excel.exe", "powerpnt.exe", "wps", "et", "wpp"),
#     emacs_dict_disable_alt,
#     "Emacs-like keys without alt")

# * Keybindings for Remmina
remmina_dict = emacs_dict.copy()
remmina_dict.update(firefox_dict)
define_keymap(
    lambda wm_class: wm_class and wm_class[-1] in ("org.remmina.Remmina",) and wm_class[0] in ("localhost:5901", "Epyc", "Remmina Remote Desktop Client"),
    remmina_dict,
    "Remmina")

# * Keybindings for llpp
define_keymap(
    lambda wm_class: wm_class and wm_class[-1] in ("llpp",),
    {
        K("k"): with_mark(K("up")),
        K("j"): with_mark(K("down"))
    },
    "llpp")

# * Keybindings for URxvt
define_keymap(
    lambda wm_class: wm_class and wm_class[-1] in ("URxvt"),
    {
        K("C-Super-enter"): K("Shift-down"),
        K("C-M-TAB"): K("Shift-right"),
        K("C-M-grave"): K("Shift-left"),
    },
    "URxvt")
