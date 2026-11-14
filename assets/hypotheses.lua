-- hypotheses.lua
-- Auto-numbers hypotheses and sub-hypotheses from a list in _quarto.yml
-- and turns references like @hyp-demand-constituents into "Hypothesis 1A".

local labels = {}   -- id -> number, e.g. "hyp-demand" -> "1", "hyp-demand-need" -> "1B"
local present = {}  -- hypothesis ids defined in this document (link targets)
local letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

local function str(x) return pandoc.utils.stringify(x) end

local function warn(msg) io.stderr:write("[hypotheses.lua] " .. msg .. "\n") end

-- each sub-hypothesis can be a plain name, or a map with
--   id:    its name
--   label: optional fixed suffix, e.g. "+" gives "3A(+)" instead of "3A.1"
--   subs:  optional list of sub-sub-hypotheses (same format)
local function add_subs(prefix_id, prefix_num, subs, depth)
  for j, s in ipairs(subs) do
    local name, custom, kids
    if type(s) == "table" and s.id then
      name, custom, kids = str(s.id), s.label and str(s.label), s.subs
    else
      name = str(s)
    end
    local id = prefix_id .. "-" .. name
    local num
    if custom then
      num = prefix_num .. "(" .. custom .. ")"
    elseif depth == 1 then
      num = prefix_num .. letters:sub(j, j)
    else
      num = prefix_num .. "." .. tostring(j)
    end
    labels[id] = num
    if kids then add_subs(id, num, kids, depth + 1) end
  end
end

local function build_labels(meta)
  if not meta.hypotheses then
    warn("no 'hypotheses' list found in metadata")
    return
  end
  for i, h in ipairs(meta.hypotheses) do
    local pid = "hyp-" .. str(h.id)
    labels[pid] = tostring(i)
    if h.subs then add_subs(pid, tostring(i), h.subs, 1) end
  end
end

local function collect_ids(div)
  if div.classes:includes("hypothesis") and div.identifier ~= "" then
    present[div.identifier] = true
  end
end

local function format_div(div)
  if not div.classes:includes("hypothesis") then return nil end
  -- a div with an id defines a hypothesis; a div with ref="..." restates one
  local key = div.identifier ~= "" and div.identifier or div.attributes["ref"]
  local num = key and labels[key]
  if not num then
    warn("no entry in _quarto.yml for '" .. tostring(key) .. "'")
    num = "??"
  end
  local name = div.attributes["name"]
  local lead = "Hypothesis " .. num .. (name and (" (" .. name .. ")") or "") .. "."
  local first = div.content[1]
  if first and (first.t == "Para" or first.t == "Plain") then
    first.content:insert(1, pandoc.Space())
    first.content:insert(1, pandoc.Strong(pandoc.Inlines(lead)))
  else
    div.content:insert(1, pandoc.Para({ pandoc.Strong(pandoc.Inlines(lead)) }))
  end
  div.attributes["name"] = nil
  div.attributes["ref"] = nil
  -- set the hypothesis off as a block quote in both HTML and PDF
  return pandoc.Div({ pandoc.BlockQuote(div.content) }, div.attr)
end

local function format_cite(cite)
  local out = pandoc.Inlines({})
  for k, c in ipairs(cite.citations) do
    if not c.id:match("^hyp%-") then return nil end  -- leave real citations alone
    local num = labels[c.id]
    if not num then
      warn("unknown hypothesis reference '@" .. c.id .. "'")
      num = "??"
    end
    local text = pandoc.Inlines("Hypothesis " .. num)
    if k > 1 then out:extend(pandoc.Inlines(", ")) end
    if present[c.id] then
      out:insert(pandoc.Link(text, "#" .. c.id))
    else
      out:extend(text)  -- target is in another chapter (HTML): plain text
    end
  end
  return out
end

return {
  { Meta = build_labels },
  { Div = collect_ids },
  { Div = format_div, Cite = format_cite },
}
