package.path = "../utils/?.lua;" .. package.path

local loop = require "loop"

local fun = require "fun"
local concat = table.concat
local getlocal = debug.getlocal

local IterationVector = {}

-- Metatable for iteration and dependence distance vectors
local obj_mt
obj_mt = {
    __index = IterationVector,

    -- Subtracting two iteration vectors gives a distance vector
    __sub = function (x, y)
        return setmetatable(fun.map2(fun['-'], x, y), obj_mt)
    end,

    __tostring = function (x)
        return "(" .. concat(x, ", ") .. ")"
    end
}

local class_mt = {
    __call = function ()
        return setmetatable({}, obj_mt)
    end
}

setmetatable(IterationVector, class_mt)

function IterationVector:current()
    local t = IterationVector()
    local i = 1
    while true do
        local name = getlocal(3, i)
        if not name then break end
        if name == "(for generator)" then
            assert(getlocal(3, i + 1) == "(for state)")
            assert(getlocal(3, i + 2) == "(for control)")
            t[#t + 1] = select(2, getlocal(3, i + 3))
            i = i + 3
        end
        i = i + 1
    end
    return t
end

function IterationVector:normalize()
    assert(#self == #loop.loops)
    for level, loop in ipairs(loop.loops) do
        self[level] = math.floor((self[level] - loop.lo) / loop.step)
    end
    return self
end

return IterationVector
