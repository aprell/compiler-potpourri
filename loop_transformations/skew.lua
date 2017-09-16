require "loop"

local simplify = require "simplify"

-- Loop skewing with factor f
function skew(stmt, f)
    assert(stmt ~= nil and stmt.body ~= nil)
    local outer, inner = stmt, stmt.body
    local outer_is, inner_is = outer.it .. "s", inner.it .. "s"
    local inner_lo = string.format("%s + %s",
        f == 1 and outer_is or f .. "*" .. outer_is, inner.lo)
    local inner_hi = string.format("%s + %s",
        f == 1 and outer_is or f .. "*" .. outer_is, inner.hi)
    local inner_body = string.format("%s = %s; %s = %s; ...",
        outer.it, outer_is, inner.it,
        f == 1 and inner_is .. " - " .. outer_is or
        inner_is .. " - " .. f .. "*" .. outer_is)
    local skewed =
        loop (outer_is, outer.lo, outer.hi, outer.step) {
            loop (inner_is, inner_lo, inner_hi, inner.step) {
                inner_body
            }
        }
    skewed.body.f = f
    return skewed
end

-- Special-purpose loop interchange
local function interchange(stmt)
    assert(stmt ~= nil and stmt.body ~= nil)
    local outer, inner = stmt, stmt.body
    local l_2 = inner.lo:match("+ (.*)")
    local u_2 = inner.hi:match("+ (.*)")
    local f = inner.f or 1
    local inner_lo = simplify(inner.lo:gsub(outer.it, "(" .. outer.lo .. ")"))
    local inner_hi = simplify(inner.hi:gsub(outer.it, "(" .. outer.hi .. ")"))
    local outer_lo = simplify(string.format("max(%s, %s)", outer.lo,
        f == 1 and ("%s - (%s)"):format(inner.it, u_2) or
        ("math.ceil((%s - (%s)) / %s)"):format(inner.it, u_2, f)))
    local outer_hi = simplify(string.format("min(%s, %s)", outer.hi,
        f == 1 and ("%s - (%s)"):format(inner.it, l_2) or
        ("math.floor((%s - (%s)) / %s)"):format(inner.it, l_2, f)))
    local interchanged =
        loop (inner.it, inner_lo, inner_hi, inner.step) {
            loop (outer.it, outer_lo, outer_hi, outer.step) {
                inner.body
            }
        }
    return interchanged
end

function skew_interchange(stmt, f)
    return interchange(skew(stmt, f))
end
