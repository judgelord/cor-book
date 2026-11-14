-- rowlines.lua: thin horizontal rules between table rows in PDF output.
-- Applies to tables whose ID is listed under `rowlines:` in the metadata,
-- or that carry the class .rowlines. Runs after Quarto; HTML is untouched.

local targets = {}

local function add_rules(latex)
  local lines, out = {}, {}
  for line in (latex .. "\n"):gmatch("(.-)\n") do lines[#lines + 1] = line end
  for i, line in ipairs(lines) do
    local nxt = lines[i + 1] or ""
    if line:match(" \\\\$") and not nxt:match("^\\") then
      out[#out + 1] = line .. " \\midrule[0.2pt]\\noalign{}"
    else
      out[#out + 1] = line
    end
  end
  return table.concat(out, "\n")
end

local function wants(t)
  return t.classes:includes("rowlines") or targets[t.identifier]
end

return {
  { Meta = function(meta)
      if meta.rowlines then
        for _, id in ipairs(meta.rowlines) do targets[pandoc.utils.stringify(id)] = true end
      end
    end },
  { Table = function(t)
      if not FORMAT:match("latex") then return nil end
      if not wants(t) then return nil end
      local latex = pandoc.write(pandoc.Pandoc({ t }), "latex")
      return pandoc.RawBlock("latex", add_rules(latex))
    end },
}
