-- Lite XL user config. Linked to ~/.config/lite-xl/init.lua by `make lite-xl`.
-- Colors and font come from plugins/omarchy.lua, which follows the Omarchy theme.

local core = require "core"
local config = require "core.config"
local keymap = require "core.keymap"

-- Editing
config.indent_size = 4
config.tab_type = "soft"
config.line_limit = 100
config.scroll_past_end = true
config.highlight_current_line = true
config.line_height = 1.25
config.max_log_items = 200

-- Quiet the UI
config.transitions = false
config.animate_drag_scroll = false

-- Also hide these folders from the project tree and fuzzy finder
for _, pattern in ipairs { "^%.venv/", "^target/", "^dist/", "^build/", "^%.direnv/" } do
  table.insert(config.ignore_files, pattern)
end

-- Bundled plugins
config.plugins.trimwhitespace = { enabled = true, trim_empty_end_lines = true }
config.plugins.lineguide = { enabled = true, rulers = { config.line_limit } }
config.plugins.drawwhitespace = { enabled = false }
config.plugins.toolbarview = false
config.plugins.linewrapping = { mode = "word", guide = false }

-- Sublime-style keys that Lite XL lacks by default
-- (ctrl+shift+p, ctrl+p, ctrl+d, ctrl+shift+d, ctrl+shift+k already match)
keymap.add {
  ["ctrl+k ctrl+b"] = "treeview:toggle",
  ["alt+z"]         = "line-wrapping:toggle",
}

core.log_quiet("Loaded init.lua")
