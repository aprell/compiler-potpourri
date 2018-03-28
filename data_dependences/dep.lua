package.path = "../utils/?.lua;" .. package.path

local fun = require "fun"
local concat = table.concat
local getlocal = debug.getlocal
local unpack = unpack or table.unpack

-- Verbose output with -v or --verbose
local output = arg[1] or "default"

local mt = {
    __index = function (a, i)
        table.insert(a.last_ref, i)
        return a
    end
}

-- Constructor for arrays
function array(name)
    local a = {name = name, array = {}, last_ref = {}}
    return setmetatable(a, mt)
end

-- Hack hack
local function get_current_iteration()
    local t = {}
    local i = 1
    while true do
        local name, _ = getlocal(3, i)
        if not name then break end
        if name == "(for index)" then
            assert(getlocal(3, i + 1) == "(for limit)")
            assert(getlocal(3, i + 2) == "(for step)")
            t[#t + 1] = select(2, getlocal(3, i + 3))
            i = i + 3
        end
        i = i + 1
    end
    return t
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
    local curr_iter = get_current_iteration()
    local index = concat(a.last_ref, "_")
    local value = a.array[index]
    if value ~= nil then
        if value.last_def ~= nil then
            -- Write After Write
            local prev_stmt, prev_iter = unpack(value.last_def)
            local dist = fun.map2(fun["-"], curr_iter, prev_iter)
            if output == "-v" or output == "--verbose" then
                printf("Output dependence in iteration %s, %s: %s written in iteration %s by %s",
                "(" .. concat(curr_iter, ", ") .. ")", curr_stmt,
                a.name .. "[" .. concat(a.last_ref, "][") .. "]",
                "(" .. concat(prev_iter, ", ") .. ")", prev_stmt)
            else
                printf("%s output %s, d = %s",
                prev_stmt, curr_stmt, "(" .. concat(dist, ", ") .. ")")
            end
        end
        if value.last_use ~= nil then
            -- Write After Read
            local prev_stmt, prev_iter = unpack(value.last_use)
            local dist = fun.map2(fun["-"], curr_iter, prev_iter)
            if output == "-v" or output == "--verbose" then
                printf("Anti dependence in iteration %s, %s: %s read in iteration %s by %s",
                "(" .. concat(curr_iter, ", ") .. ")", curr_stmt,
                a.name .. "[" .. concat(a.last_ref, "][") .. "]",
                "(" .. concat(prev_iter, ", ") .. ")", prev_stmt)
            else
                printf("%s anti %s, d = %s",
                prev_stmt, curr_stmt, "(" .. concat(dist, ", ") .. ")")
            end
        end
    else
        a.array[index] = {}
    end
    a.array[index].last_def = {curr_stmt, curr_iter}
    clear(a.last_ref)
end

function use(a)
    local curr_iter = get_current_iteration()
    local index = concat(a.last_ref, "_")
    local value = a.array[index]
    if value ~= nil then
        if value.last_def ~= nil then
            -- Read After Write
            local prev_stmt, prev_iter = unpack(value.last_def)
            local dist = fun.map2(fun["-"], curr_iter, prev_iter)
            if output == "-v" or output == "--verbose" then
                printf("Flow dependence in iteration %s, %s: %s written in iteration %s by %s",
                "(" .. concat(curr_iter, ", ") .. ")", curr_stmt,
                a.name .. "[" .. concat(a.last_ref, "][") .. "]",
                "(" .. concat(prev_iter, ", ") .. ")", prev_stmt)
            else
                printf("%s flow %s, d = %s",
                prev_stmt, curr_stmt, "(" .. concat(dist, ", ") .. ")")
            end
        end
    else
        a.array[index] = {}
    end
    a.array[index].last_use = {curr_stmt, curr_iter}
    clear(a.last_ref)
end
