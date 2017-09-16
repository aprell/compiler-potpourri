local function indent(lvl, width)
    return string.rep(" ", lvl * (width or 4))
end

local function indent_stmt(stmt, lvl)
    return "\n" .. indent(lvl) .. stmt .. "\n" .. indent(lvl - 1)
end

local function to_string(loop, lvl)
    if type(loop) == "string" then return loop end
    lvl = lvl or 1
    local it, lo, hi, step = loop.it, loop.lo, loop.hi, loop.step
    if tonumber(lo) == nil or tonumber(hi) == nil or lo <= hi then
        assert(step > 0, "Step size must be positive")
        local s = "for (%s = %s; %s <= %s; %s%s) {%s}"
        return s:format(it, lo, it, hi, it,
        step == 1 and "++" or " += " .. tostring(step),
        indent_stmt(to_string(loop.body or "...", lvl + 1), lvl))
    else
        assert(step < 0, "Step size must be negative")
        local s = "for (%s = %s; %s >= %s; %s%s) {%s}"
        return s:format(it, lo, it, hi, it,
        step == -1 and "--" or " -= " .. tostring(math.abs(step)),
        indent_stmt(to_string(loop.body or "...", lvl + 1), lvl))
    end
end

local mt = {
    __tostring = to_string
}

-- Constructor for loop nests
function loop(it, lo, hi, step)
    local l = {it = it, lo = lo, hi = hi, step = step or 1}
    l.ty = "loop"
    setmetatable(l, mt)
    return function (body)
        if type(body) == "string" then
            l.body = body
        elseif type(body) == "table" and type(body[1]) == "string" then
            l.body = body[1]
        else
            assert(type(body) == "table")
            l.body = unpack(body)
            l.body.ty = "loop"
            assert(type(l.body) == "table")
        end
        return l
    end
end
