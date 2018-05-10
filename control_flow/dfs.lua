package.path = "../utils/?.lua;" .. package.path

local Set = require "set"

function invert(graph)
    local inv = {}
    for name, node in pairs(graph) do
        if name == "entry" then name = "exit"
        elseif name == "exit" then name = "entry" end
        inv[name] = {succ = node.pred, pred = node.succ}
    end
    return inv
end

function dfs_preorder(graph, name)
    name = name or "entry"
    visited = Set {}
    order = {}
    local function visit(name)
        visited:add(name)
        if name == "exit" then return end
        if name ~= "entry" then
            table.insert(order, name)
        end
        for _, succ in ipairs(graph[name].succ) do
            if not visited[succ] then visit(succ) end
        end
    end
    visit(name)
    return order
end

function dfs_postorder(graph, name)
    name = name or "entry"
    visited = Set {}
    order = {}
    local function visit(name)
        visited:add(name)
        if name == "exit" then return end
        for _, succ in ipairs(graph[name].succ) do
            if not visited[succ] then visit(succ) end
        end
        if name ~= "entry" then
            table.insert(order, name)
        end
    end
    visit(name)
    return order
end

-- Topologically sorts the nodes of a DAG
function dfs_reverse_postorder(graph, name)
    name = name or "entry"
    visited = Set {}
    order = {}
    local function visit(name)
        visited:add(name)
        if name == "exit" then return end
        for _, succ in ipairs(graph[name].succ) do
            if not visited[succ] then visit(succ) end
        end
        if name ~= "entry" then
            table.insert(order, 1, name)
        end
    end
    visit(name)
    return order
end
