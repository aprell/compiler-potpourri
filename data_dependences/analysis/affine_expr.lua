package.path = "../../utils/?.lua;" .. package.path

local fun = require "fun"
local parse = require "parse"

local literal = parse.literal
local number = parse.number
local variable = parse.variable

--+----------------+
--| Constant terms |
--+----------------+

local ConstantTerm = {}

function ConstantTerm.eval(a, b, op)
    if type(a) == "table" and type(b) == "table" then
        assert(type(a[1]) == "number")
        assert(type(b[1]) == "number")
        return setmetatable({op(a[1], b[1])}, getmetatable(a))
    elseif type(b) == "table" then
        a, b = b, a
    end
    assert(type(a) == "table")
    assert(type(a[1]) == "number")
    return setmetatable({op(a[1], b)}, getmetatable(a))
end

local obj_mt = {
    __index = ConstantTerm,

    __tostring = function (term)
        return tostring(term[1])
    end,

    __add = function (a, b) return ConstantTerm.eval(a, b, fun["+"]) end,
    __sub = function (a, b) return ConstantTerm.eval(a, b, fun["-"]) end,
    __mul = function (a, b) return ConstantTerm.eval(a, b, fun["*"]) end,
    __unm = function (a) return -1 * a end
}

local class_mt = {
    __call = function (t, s)
        return t.constant_term:match(s)
    end
}

setmetatable(ConstantTerm, class_mt)

ConstantTerm.constant_term = parse.Ct (
    number + variable
) /
function (expr)
    return setmetatable(expr, obj_mt)
end

do
    local a = ConstantTerm "1"
    local b = ConstantTerm "3"
    local c = ConstantTerm "5"
    assert((a + 2 * b - 4 * -c)[1] == 27)
end

--+--------------+
--| Linear terms |
--+--------------+

local LinearTerm = {}

function LinearTerm.eval(a, b, op)
    if type(a) == "table" and type(b) == "table" then
        assert(a[2] == b[2])
        return setmetatable({op(a[1], b[1]), a[2]}, getmetatable(a))
    elseif type(b) == "table" then
        a, b = b, a
    end
    assert(type(a) == "table")
    return setmetatable({op(a[1], b), a[2]}, getmetatable(a))
end

local obj_mt = {
    __index = LinearTerm,

    __tostring = function (term)
        return (term[1] == 1 and "" or term[1] == -1 and "-" or term[1]) .. term[2]
    end,

    __add = function (a, b) return LinearTerm.eval(a, b, fun["+"]) end,
    __sub = function (a, b) return LinearTerm.eval(a, b, fun["-"]) end,
    __mul = function (a, b) return LinearTerm.eval(a, b, fun["*"]) end,
    __unm = function (a) return -1 * a end
}

local class_mt = {
    __call = function (t, s)
        return t.linear_term:match(s)
    end
}

setmetatable(LinearTerm, class_mt)

LinearTerm.linear_term = parse.Ct (
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

do
    local a = LinearTerm "i"
    local b = LinearTerm "2*i"
    local c = LinearTerm "4*i"
    assert(((a + 1) * b - 2 * -c + a)[1] == 13)
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
        local s = {}
        local i = 1
        while i <= #expr do
            -- a + -b -> a - b
            if expr[i] == "+" and expr[i+1][1] < 0 then
                s[#s+1] = "-"
                s[#s+1] = tostring(-expr[i+1])
                i = i + 2
            else
                s[#s+1] = tostring(expr[i])
                i = i + 1
            end
        end
        return table.concat(s, " ")
    end
}

local class_mt = {
    __call = function (t, s)
        return t.affine_expr:match(s)
    end
}

setmetatable(AffineExpr, class_mt)

AffineExpr.affine_expr = parse.Ct (
    LinearTerm.linear_term *
    ((literal "+" + literal "-") * LinearTerm.linear_term) ^ 0 *
    ((literal "+" + literal "-") * ConstantTerm.constant_term) ^ -1 +
    ConstantTerm.constant_term
) /
function (expr)
    for i = 1, #expr do
        -- "a - b" -> "a + -b"
        if expr[i] == "-" then
            expr[i] = "+"
            expr[i+1] = -expr[i+1]
        end
    end
    return setmetatable(expr, obj_mt)
end

-- To understand why the following two functions work, keep in mind that
-- ("+")[1] or ("+")[2] evaluate to nil, because getmetatable("+").__index does
-- not contain integer keys.

function AffineExpr:coefficients()
    return fun.map(fun.fst, self)
end

function AffineExpr:variables()
    return fun.map(fun.snd, self)
end

function AffineExpr:linear_terms()
    return fun.filter(function (term)
        return type(term) == "table" and #term == 2
    end, self)
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

local e2 = AffineExpr "2*i - 3*j + 4*k - 5"

assert(e2:coefficients()[1] ==  2)
assert(e2:coefficients()[2] == -3)
assert(e2:coefficients()[3] ==  4)
assert(e2:coefficients()[4] == -5)
assert(e2:variables()[1] == "i")
assert(e2:variables()[2] == "j")
assert(e2:variables()[3] == "k")
assert(e2:constant() == -5)
assert(e2:class() == "MIV")

return AffineExpr
