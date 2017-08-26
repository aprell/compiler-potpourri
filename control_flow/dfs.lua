function invert(graph)
    local inv = {}
    for name, node in pairs(graph) do
        if name == "entry" then name = "exit"
        elseif name == "exit" then name = "entry" end
        inv[name] = {succ = node.pred, pred = node.succ}
    end
    return inv
end

function mark_unvisited(graph)
    for _, node in pairs(graph) do
        node.visited = false
    end
    return graph
end

function dfs(graph, name, nodes)
    name = name or "entry"
    nodes = nodes or {}
    if name == "exit" then return nodes end
    local node = graph[name]
    if not node.visited then
        node.visited = true
        for _, succ in ipairs(node.succ) do
            dfs(graph, succ, nodes)
        end
        if name ~= "entry" then
            table.insert(nodes, 1, name)
        end
    end
    return nodes
end
