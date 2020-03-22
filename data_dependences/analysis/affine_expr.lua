package.path = "../../utils/?.lua;" .. package.path

local fun = require "fun"
local parse = require "parse"

local literal = parse.literal
local number = parse.number
local variable = parse.variable

local constant_term = parse.Ct (
    number + variable
)

--+--------------+
--| Linear terms |
--+--------------+

local LinearTerm = {}

local obj_mt = {
    __index = LinearTerm,

    __tostring = function (term)
        return (term[1] ~= 1 and term[1] or "") .. term[2]
    end
}

local linear_term = parse.Ct (
    (number * (literal "*" / 0)) ^ -1 * variable
) /
function (expr)
    if #expr == 1 then
        return setmetatable({1, expr[1]}, obj_mt)
    else
        assert(#expr == 2)
        return setmetatable(expr, obj_mt)
    end
end

--+--------------------+
--| Affine expressions |
--+--------------------+

-- An affine expression has the form
--
--     a_1 * i_1 + a_2 * i_2 + ... + a_n * i_n + e,
--
-- where i_k is the index for the loop at nesting level k, all a_k, 1 <= k <=
-- n, are integer constants, and e is a loop-invariant expression or constant.

local AffineExpr = {}

local obj_mt = {
    __index = AffineExpr,

    __tostring = function (expr)
        return table.concat(fun.map(function (e)
            if #e == 2 then
                if e[1] == 1 then return e[2]
                else return e[1] .. e[2] end
            else
                assert(#e == 1)
                return e[1]
            end
        end, expr), " + ")
    end
}

local class_mt = {
    __call = function (t, s)
        return t.affine_expr:match(s)
    end
}

setmetatable(AffineExpr, class_mt)

AffineExpr.affine_expr = parse.Ct (
    linear_term *
    ((literal "+" / 0) * linear_term) ^ 0 *
    ((literal "+" / 0) * constant_term) ^ -1 +
    constant_term
) /
function (expr)
    return setmetatable(expr, obj_mt)
end

function AffineExpr:coefficients()
    return fun.map(fun.fst, self)
end

function AffineExpr:variables()
    return fun.map(fun.snd, self)
end

function AffineExpr:constant()
    if #self[#self] == 1 then return self[#self][1] else return 0 end
end

-- An affine expression becomes more complex the more index variables it
-- contains. We distinguish between ZIV (zero index variables), SIV (single
-- index variable), and MIV (multiple index variables).
function AffineExpr:class()
    local vars = self:variables()
    if #vars == 0 then return "ZIV", 0
    elseif #vars == 1 then return "SIV", 1
    else return "MIV", #vars end
end

-- Try to simplify an affine expression with zero index variables (ZIV)
function AffineExpr:simplify()
    if self:class() ~= "ZIV" then return self end

    local function eval(expr)
        local ok, val = pcall(load("return " .. expr))
        return ok and val or expr
    end

    return eval(self[1][1])
end

local e0 = AffineExpr "0"

assert(e0:coefficients()[1] == 0)
assert(e0:variables()[1] == nil)
assert(e0:constant() == 0)
assert(e0:class() == "ZIV")

local e1 = AffineExpr "i"

assert(e1:coefficients()[1] == 1)
assert(e1:variables()[1] == "i")
assert(e1:constant() == 0)
assert(e1:class() == "SIV")

local e2 = AffineExpr "2*i + -3*j + -4"

assert(e2:coefficients()[1] ==  2)
assert(e2:coefficients()[2] == -3)
assert(e2:coefficients()[3] == -4)
assert(e2:variables()[1] == "i")
assert(e2:variables()[2] == "j")
assert(e2:constant() == -4)
assert(e2:class() == "MIV")

return AffineExpr
