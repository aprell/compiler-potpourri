package.path = "../../utils/?.lua;" .. package.path

local fun = require "fun"
local parse = require "parse"

local literal = parse.literal
local number = parse.number
local variable = parse.variable

local StridedRange = require "strided_range"

--+-------------+
--| Loop bounds |
--+-------------+

local LoopBounds = {}

local obj_mt = {
    __index = LoopBounds
}

local class_mt = {
    __call = function (t, s)
        return t.bounds:match(s)
    end
}

setmetatable(LoopBounds, class_mt)

LoopBounds.bounds = parse.Ct (
    -- lower bound
    (number + variable) * (literal "<=" / 0) *
    -- loop index variable
    variable * (literal "<=" / 0) *
    -- upper bound
    (number + variable)
) /
function (expr)
    return setmetatable({
        variable = expr[2],
        bounds = StridedRange {
            lower_bound = expr[1],
            upper_bound = expr[3],
            -- step = 1
        }
    }, obj_mt)
end

function LoopBounds.collect(...)
    return fun.fold(function (bounds, loop)
        bounds[loop.variable] = loop.bounds
        return bounds
    end, {}, {...})
end

local i = LoopBounds "1 <= i <= 10"
local j = LoopBounds "2 <= j <= 20"
local k = LoopBounds "3 <= k <= 30"

local ijk = LoopBounds.collect(i, j, k)

assert(ijk.i.lower_bound == 1)
assert(ijk.i.upper_bound == 10)
assert(ijk.j.lower_bound == 2)
assert(ijk.j.upper_bound == 20)
assert(ijk.k.lower_bound == 3)
assert(ijk.k.upper_bound == 30)

return LoopBounds
