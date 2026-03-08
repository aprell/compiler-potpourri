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

-- Detect the control variables of for loops
local FOR_1, FOR_2, FOR_3

local function probe_control_variables()
    for _ = 1, 1 do
        FOR_1 = debug.getlocal(1, 1)
        FOR_2 = debug.getlocal(1, 2)
        FOR_3 = debug.getlocal(1, 3)
        if FOR_1 == "(for generator)" then
            -- Pre-Lua 5.4
            assert(FOR_2 == "(for state)")
            assert(FOR_3 == "(for control)")
        else
            -- Lua 5.4
            assert(FOR_1 == "(for state)")
            assert(FOR_2 == "(for state)")
            assert(FOR_3 == "(for state)")
        end
    end
end

probe_control_variables()

function IterationVector:current()
    local t = IterationVector()
    local i = 1
    while true do
        local name = getlocal(3, i)
        if not name then break end
        if name == FOR_1 then
            assert(getlocal(3, i + 1) == FOR_2)
            assert(getlocal(3, i + 2) == FOR_3)
            -- Lua 5.4 generic for loop?
            if getlocal(3, i + 3) == FOR_2 then i = i + 1 end
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
