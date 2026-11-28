-- mod-version:3
-- Follows the Omarchy theme and monospace font.
--
-- Colors come from `omarchy-theme-color --all`, the font from
-- `omarchy font current` resolved to a file with fc-match. The theme-set and
-- font-set hooks touch STAMP (see .config/omarchy/hooks/lite-xl-sync), and a
-- background thread re-syncs when it changes, so a running editor follows
-- `omarchy theme set` and `omarchy font set`. Without Omarchy nothing changes.

local core = require "core"
local common = require "core.common"
local command = require "core.command"
local style = require "core.style"

local STAMP = (os.getenv("XDG_STATE_HOME") or (HOME .. "/.local/state"))
  .. "/lite-xl/omarchy.stamp"

local function read_lines(cmd)
  local f = io.popen(cmd .. " 2>/dev/null")
  if not f then return {} end
  local lines = {}
  for line in f:lines() do lines[#lines + 1] = line end
  f:close()
  return lines
end

local function palette()
  local c = {}
  for _, line in ipairs(read_lines("omarchy-theme-color --all")) do
    local k, v = line:match("^([%w_]+)\t(#%x%x%x%x%x%x)$")
    if k then c[k] = v end
  end
  return c.background and c or nil
end

-- Mix two "#rrggbb" colors; amount is the share of `to` (0..1).
local function mix(from, to, amount)
  local a, b = { common.color(from) }, { common.color(to) }
  local function ch(i) return math.floor(a[i] + (b[i] - a[i]) * amount + 0.5) end
  return string.format("#%02x%02x%02x", ch(1), ch(2), ch(3))
end

local function apply_colors(c)
  local function set(key, hex) style[key] = { common.color(hex) } end
  set("background", c.background)
  set("background2", c.dark_background)
  set("background3", c.dark_background)
  set("text", c.dark_foreground)
  set("caret", c.cursor)
  set("accent", c.bright_foreground)
  set("dim", c.muted)
  set("divider", c.darker_background)
  set("selection", c.selection_background)
  set("line_number", mix(c.muted, c.background, 0.4))
  set("line_number2", c.foreground)
  set("line_highlight", mix(c.background, c.lighter_background, 0.5))
  set("scrollbar", c.lighter_background)
  set("scrollbar2", c.muted)
  set("scrollbar_track", c.dark_background)
  set("nagbar", c.red)
  set("nagbar_text", c.background)
  set("drag_overlay_tab", c.accent)
  set("good", c.green)
  set("warn", c.orange)
  set("error", c.red)
  set("modified", c.accent)
  set("guide", c.lighter_background)

  local syn = {
    normal = c.foreground,
    symbol = c.foreground,
    comment = c.muted,
    keyword = c.magenta,
    keyword2 = c.red,
    number = c.orange,
    literal = c.orange,
    string = c.green,
    operator = c.cyan,
    ["function"] = c.blue,
  }
  for k, hex in pairs(syn) do style.syntax[k] = { common.color(hex) } end

  style.log.INFO.color = style.text
  style.log.WARN.color = style.warn
  style.log.ERROR.color = style.error
end

local current_font

local function font_file(name, variant)
  local out = read_lines(string.format("fc-match -f '%%{file}' %q",
    name .. ":style=" .. variant))[1]
  return out and out ~= "" and out or nil
end

local function apply_font()
  local name = read_lines("omarchy font current")[1]
  if not name or name == "" or name == current_font then return end
  local path = font_file(name, "Regular")
  if not path then return end
  local ok, code = pcall(renderer.font.load, path, style.code_font:get_size())
  if not ok then
    core.error("omarchy: cannot load font %s", path)
    return
  end
  local ui = renderer.font.load(path, style.font:get_size())
  style.code_font = code
  style.font = ui
  style.big_font = ui:copy(style.big_font:get_size())
  current_font = name
end

local function sync()
  local c = palette()
  if c then apply_colors(c) end
  apply_font()
  core.redraw = true
end

sync()

local function stamp_mtime()
  local info = system.get_file_info(STAMP)
  return info and info.modified or false
end

local last_stamp = stamp_mtime()
core.add_thread(function()
  while true do
    local mtime = stamp_mtime()
    if mtime ~= last_stamp then
      last_stamp = mtime
      sync()
    end
    coroutine.yield(1)
  end
end)

command.add(nil, {
  ["omarchy:sync"] = function()
    current_font = nil
    sync()
  end,
})
