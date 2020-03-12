local IterationVector = require "iteration_vector"

local concat = table.concat
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
    local curr_iter = IterationVector:current()
    if normalize_iteration_vectors then
        curr_iter = curr_iter:normalize()
    end
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
    local curr_iter = IterationVector:current()
    if normalize_iteration_vectors then
        curr_iter = curr_iter:normalize()
    end
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
