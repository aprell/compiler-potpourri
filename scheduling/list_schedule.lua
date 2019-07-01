package.path = "../utils/?.lua;" .. package.path

local fun = require "fun"

local map = fun.map
local concat = table.concat
local unpack = unpack or table.unpack

local function sum(t)
    return fun.fold(fun["+"], 0, t)
end

local function min(t)
    assert(#t > 0)
    return math.min(unpack(t))
end

local function max(t)
    assert(#t > 0)
    return math.max(unpack(t))
end

local function id(node)
    return node.id
end

local function printf(...)
    print(string.format(...))
end

function find_remaining(graph)
    local W = {}

    local function printW()
        printf("W = {%s}", concat(map(id, W), ", "))
    end

    print("Initialization")
    print("--------------")
    for _, node in ipairs(graph) do
        node.count = #node.succ
        node.remaining = node.delay
        if node.count == 0 then
            table.insert(W, node)
        end
        printf("Instruction %2s: count = %2d, remaining = %2d",
            node.id, node.count, node.remaining)
    end
    printW()
    local i = 1
    while #W > 0 do
        printf("\nIteration %2d", i)
        print("------------")
        -- Remove an arbitrary instruction from W
        local x = table.remove(W)
        printf("Remove instruction %s with %s", x.id,
            #x.pred > 1 and "predecessors " .. concat(x.pred, ", ") or
            #x.pred == 1 and "predecessor " .. concat(x.pred, ", ") or
            "no predecessor")
        for _, p in ipairs(x.pred) do
            p = graph[tonumber(p)]
            local prev_remaining = p.remaining
            p.remaining = math.max(p.remaining, x.remaining + p.delay)
            p.count = p.count - 1
            if p.count == 0 then
                table.insert(W, p)
            end
            printf("Instruction %2s: count = %2d, remaining = max(%d, %d) = %2d",
                p.id, p.count, prev_remaining, x.remaining + p.delay, p.remaining)
        end
        printW()
        i = i + 1
    end
    print("\nResult")
    print("------")
    for _, node in ipairs(graph) do
        printf("Instruction %2d: count = %2d, remaining = %2d",
            node.id, node.count, node.remaining)
    end
end

local functional_units_

function functional_units(t)
    functional_units_ = t
end

local function advance_cycle()
    for _, unit in pairs(functional_units_) do
        unit.avail = unit.count
    end
end

local function length(sched)
    local len = 0
    for cycle, instr in ipairs(sched) do
        -- cycle is the starting time/cycle for every instruction i in instr
        -- (possibly empty if no instruction can be scheduled for execution)
        len = math.max(len, table.unpack(map(function (i)
            return cycle + i.delay-1
        end, instr)))
    end
    return len
end

local function to_string(sched)
    local t = {}
    t[#t+1] = "-------+--------------"
    t[#t+1] = " Cycle | Instructions "
    t[#t+1] = "-------+--------------"
    for cycle, instr in ipairs(sched) do
        t[#t+1] = (" %3d   |   [%s]"):format(cycle, concat(map(id, instr), ", "))
    end
    t[#t+1] = "-------+--------------"
    t[#t+1] = (" => %d cycles"):format(length(sched))
    t[#t+1] = "----------------------"
    return concat(t, "\n")
end

function list_schedule(graph)
    -- Assume all functional units can receive instructions
    advance_cycle()

    -- Determine length of work list
    local Wlen = max(map(function (node)
        return node.delay
    end, graph)) + 1

    local function inc(n)
        return n % Wlen + 1
    end

    local W = {}
    for i = 1, Wlen do
        W[i] = {}
    end

    local function printW()
        printf("W = {%s}", concat(map(function (list)
            return "{" .. concat(map(id, list), ", ") .. "}"
        end, W), ", "))
    end

    -- Override '#' for the work list (a list of lists) to return the number of
    -- contained instructions => maintaining a separate count becomes redundant
    setmetatable(W, {
        __len = function (W)
            return sum(map(function (l)
                return #l
            end, W))
        end
    })

    local c, cW = 1, 1
    local instr = {}
    instr[c] = {}

    local function print_instr()
        printf("instr = [%s]", concat(map(function (list)
            return "[" .. concat(map(id, list), ", ") .. "]"
        end, instr), ", "))
    end

    print("Initialization")
    print("--------------")

    -- Initialize work list
    for _, node in ipairs(graph) do
        node.count = #node.pred
        if node.count == 0 then
            table.insert(W[1], node)
        end
        printf("Instruction %2s: count = %2d, remaining = %2d",
            node.id, node.count, node.remaining or 0)
    end

    printW()
    printf("\nCycle %2d", c)
    print("--------")

    while #W > 0 do
        while #W[cW] == 0 do
            c = c + 1
            instr[c] = {}
            cW = inc(cW)
            -- Pipelining -> functional units can receive new instructions
            advance_cycle()
            printf("Cycle %2d", c)
            print("--------")
        end
        local nextc = inc(c)
        while #W[cW] > 0 do
            local list = W[cW]
            -- Try to remove most critical instruction from list
            -- If this information is missing, remove the instruction
            -- with the smallest node identifier
            local index = 1
            if list[1].remaining ~= nil then
                local max_remaining = list[1].remaining
                for i = 2, #list do
                    if list[i].remaining > max_remaining then
                        max_remaining = list[i].remaining
                        index = i
                    end
                end
            else
                local min_id = id(list[1])
                for i = 2, #list do
                    if id(list[i]) < min_id then
                        min_id = id(list[i])
                        index = i
                    end
                end
            end
            local x = table.remove(list, index)
            printf("Remove instruction %s with %s", x.id,
                #x.succ > 1 and "successors " .. concat(x.succ, ", ") or
                #x.succ == 1 and "successor " .. concat(x.succ, ", ") or
                "no successor")
            -- Check if instruction can be issued
            local unit = functional_units_[x.ty]
            if unit.avail > 0 then
                printf("Instruction %s of type %s can be issued", x.id, x.ty)
                unit.avail = unit.avail - 1
                table.insert(instr[c], x)
                -- Update following instructions
                for _, s in ipairs(x.succ) do
                    s = graph[tonumber(s)]
                    assert(s.count > 0)
                    local prev_earliest = s.earliest or 1
                    s.earliest = math.max(s.earliest or 1, c + x.delay)
                    s.count = s.count - 1
                    if s.count == 0 then
                        table.insert(W[inc(s.earliest - 1)], s)
                    end
                    printf("Instruction %2s: count = %2d, earliest = max(%d, %d) = %2d",
                        s.id, s.count, prev_earliest, c + x.delay, s.earliest)
                end
            else -- Delay instruction
                printf("Instruction %s of type %s cannot be issued", x.id, x.ty)
                table.insert(W[nextc], x)
            end
            print_instr()
            printW()
            print("")
        end
    end
    print("Result")
    print("------")
    for _, node in ipairs(graph) do
        printf("Instruction %2d: count = %2d, earliest = %2d",
            node.id, node.count, node.earliest or 1)
    end
    return setmetatable(instr, {__tostring = to_string})
end
