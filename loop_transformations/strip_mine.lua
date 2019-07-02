require "loop"

-- Strip mining with strip size s
function strip_mine(stmt, s)
    if stmt.ty ~= "loop" then return stmt end
    local outer = stmt
    local outer_it = outer.it .. "s"
    local inner_lo = outer_it
    local inner_hi = string.format("min(%s, %s + %s)", outer.hi, outer_it, s - 1)
    local strip_mined =
        loop (outer_it, outer.lo, outer.hi, s) {
            loop (outer.it, inner_lo, inner_hi, 1) {
                outer.body
            }
        }
    return strip_mined
end
