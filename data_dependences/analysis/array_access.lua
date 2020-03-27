package.path = "../../utils/?.lua;" .. package.path

local Set = require "set"
local fun = require "fun"
local parse = require "parse"

local literal = parse.literal
local variable = parse.variable

local AffineExpr = require "affine_expr"
local LoopBounds = require "loop_bounds"
local StridedRange = require "strided_range"

--+-------------------------+
--| Affine array references |
--+-------------------------+

-- An affine array reference is an array plus one or more affine subscript
-- expressions, one for each array dimension.

local ArrayAccess = {}

local obj_mt = {
    __index = ArrayAccess,

    __tostring = function (a)
        s = a.name
        for _, e in ipairs(a.subscripts) do
            s = s .. "[" .. tostring(e) .. "]"
        end
        return s
    end
}

local class_mt = {
    __call = function (t, s)
        return t.array_access:match(s)
    end
}

setmetatable(ArrayAccess, class_mt)

ArrayAccess.array_access = parse.Ct (
    variable * ((literal "[" / 0) * AffineExpr.affine_expr * (literal "]" / 0)) ^ 1
) /
function (expr)
    return setmetatable({
        name = expr[1],
        subscripts = {table.unpack(expr, 2)},
    }, obj_mt)
end

function ArrayAccess:region(bounds)
    if bounds == nil then return self.access_region end
    self.access_region = {}
    for dim, subscript in ipairs(self.subscripts) do
        assert(subscript:class() == "SIV")
        -- A SIV subscript has exactly one linear term
        local lterm = subscript:linear_terms()[1]
        local const = subscript:constant()
        self.access_region[dim] = StridedRange {
            lower_bound = (lterm * bounds[dim].lower_bound)[1] + const,
            upper_bound = (lterm * bounds[dim].upper_bound)[1] + const,
            step = (lterm * bounds[dim].step)[1],
        }
    end
    return self.access_region
end

local a = ArrayAccess "A[i][j][k]"

assert(a.name == "A")
assert(a.subscripts[1]:variables()[1] == "i")
assert(a.subscripts[2]:variables()[1] == "j")
assert(a.subscripts[3]:variables()[1] == "k")

local b = ArrayAccess "B[i+3][2*j][3*k-4]" : region {
    LoopBounds "0 <= i <= 10" . bounds,
    LoopBounds "0 <= j <= 20" . bounds,
    LoopBounds "0 <= k <= 30" . bounds
}

assert(tostring(b[1]) == "[3:13]")
assert(tostring(b[2]) == "[0:2:40]")
assert(tostring(b[3]) == "[-4:3:86]")

return ArrayAccess
