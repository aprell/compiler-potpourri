require "loop"

local simplify = require "simplify"

-- Loop tiling with tile size ts and tile offset to
function tile(stmt, ts, to)
    if stmt.ty ~= "loop" then return stmt end
    local outer, inner = stmt, stmt.body
    local outer_it = outer.it .. "t"
    local outer_lo = type(outer.lo) == "number" and math.floor((outer.lo - to) / ts) * ts + to or
        string.format("floor((%s - %s) / %s) * %s + %s", outer.lo, to, ts, ts, to)
    local outer_hi = type(outer.hi) == "number" and math.floor((outer.hi - to) / ts) * ts + to or
        string.format("floor((%s - %s) / %s) * %s + %s", outer.hi, to, ts, ts, to)
    local inner_lo = string.format("max(%s, %s)", outer.lo, outer_it)
    local inner_hi = string.format("min(%s, %s + %s)", outer.hi, outer_it, ts - 1)
    local tiled =
        loop (outer_it, outer_lo, outer_hi, ts) {
            loop (outer.it, inner_lo, inner_hi, 1) {
                tile(inner, ts, to)
            }
        }
    return tiled
end
