package.path = "../utils/?.lua;" .. package.path

require "range"

local fun = require "fun"
local concat = table.concat
local getlocal = debug.getlocal
local unpack = unpack or table.unpack

local verbose_output = false
local normalize_iteration_vectors = false

while #arg > 0 do
    local next_arg = arg[#arg]
    if next_arg == "-v" or next_arg == "--verbose" then
        verbose_output = true
    elseif next_arg == "-n" or next_arg == "--normalize" then
        normalize_iteration_vectors = true
    end
    table.remove(arg)
end

local array_mt = {
    __index = function (a, i)
        table.insert(a.last_ref, i)
        return a
    end
}

local array_deps_mt = {
    __tostring = function (deps)
        local t = {}
        for dep in pairs(deps) do
            t[#t + 1] = dep
        end
        return concat(t, "\n")
    end
}

-- Constructor for arrays
function array(name)
    local a = {name = name, array = {}, last_ref = {}}
    a.deps = setmetatable({}, array_deps_mt)
    return setmetatable(a, array_mt)
end

-- Metatable for iteration and dependence distance vectors
local vector_mt = {
    __tostring = function (x)
        return "(" .. concat(x, ", ") .. ")"
    end
}

-- Metatable for iteration vectors
local iteration_vector_mt = {
    -- Subtracting two iteration vectors gives a distance vector
    __sub = function (x, y)
        local d = fun.map2(fun['-'], x, y)
        return setmetatable(d, vector_mt)
    end,

    __tostring = vector_mt.__tostring
}

-- Normalize iteration vector i
local function normalize(i)
    assert(#i == #loops)
    for level, loop in ipairs(loops) do
        i[level] = math.floor((i[level] - loop.lo) / loop.step)
    end
    return i
end

local function current_iteration_vector()
    local t = setmetatable({}, iteration_vector_mt)
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
    if normalize_iteration_vectors then
        return normalize(t)
    else
        return t
    end
end

local function clear(a)
    for i = 1, #a do
        a[i] = nil
    end
end

local function printf(...)
    print(string.format(...))
end

local curr_stmt

function S(name)
    curr_stmt = name
end

function def(a)
    local curr_iter = current_iteration_vector()
    local index = concat(a.last_ref, "_")
    local value = a.array[index]
    if value ~= nil then
        if value.last_def ~= nil then
            -- Write After Write
            local prev_stmt, prev_iter = unpack(value.last_def)
            local dist = curr_iter - prev_iter
            if verbose_output then
                printf("Output dependence in iteration %s, %s: %s written in iteration %s by %s",
                curr_iter, curr_stmt,
                a.name .. "[" .. concat(a.last_ref, "][") .. "]",
                prev_iter, prev_stmt)
            end
            local dep = ("%s output %s, d = %s"):format(prev_stmt, curr_stmt, dist)
            a.deps[dep] = (a.deps[dep] or 0) + 1
        end
        if value.last_use ~= nil then
            -- Write After Read
            local prev_stmt, prev_iter = unpack(value.last_use)
            local dist = curr_iter - prev_iter
            if verbose_output then
                printf("Anti dependence in iteration %s, %s: %s read in iteration %s by %s",
                curr_iter, curr_stmt,
                a.name .. "[" .. concat(a.last_ref, "][") .. "]",
                prev_iter, prev_stmt)
            end
            local dep = ("%s anti %s, d = %s"):format(prev_stmt, curr_stmt, dist)
            a.deps[dep] = (a.deps[dep] or 0) + 1
        end
    else
        a.array[index] = {}
    end
    a.array[index].last_def = {curr_stmt, curr_iter}
    clear(a.last_ref)
end

function use(a)
    local curr_iter = current_iteration_vector()
    local index = concat(a.last_ref, "_")
    local value = a.array[index]
    if value ~= nil then
        if value.last_def ~= nil then
            -- Read After Write
            local prev_stmt, prev_iter = unpack(value.last_def)
            local dist = curr_iter - prev_iter
            if verbose_output then
                printf("Flow dependence in iteration %s, %s: %s written in iteration %s by %s",
                curr_iter, curr_stmt,
                a.name .. "[" .. concat(a.last_ref, "][") .. "]",
                prev_iter, prev_stmt)
            end
            local dep = ("%s flow %s, d = %s"):format(prev_stmt, curr_stmt, dist)
            a.deps[dep] = (a.deps[dep] or 0) + 1
        end
    else
        a.array[index] = {}
    end
    a.array[index].last_use = {curr_stmt, curr_iter}
    clear(a.last_ref)
end
