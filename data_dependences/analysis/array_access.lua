package.path = "../../utils/?.lua;" .. package.path

local Set = require "set"
local fun = require "fun"
local parse = require "parse"

local literal = parse.literal
local variable = parse.variable

local AffineExpr = require "affine_expr"

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

local a = ArrayAccess "A[i][j][k]"

assert(a.name == "A")
assert(a.subscripts[1]:variables()[1] == "i")
assert(a.subscripts[2]:variables()[1] == "j")
assert(a.subscripts[3]:variables()[1] == "k")

return ArrayAccess
