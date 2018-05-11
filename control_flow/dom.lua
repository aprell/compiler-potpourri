package.path = "../utils/?.lua;" .. package.path

require "dfs"

local Set = require "set"

function dominators(graph)
    print("Computing dominators")
    local traversal = dfs_reverse_postorder(graph)
    -- Initialization
    for i, name in ipairs(traversal) do
        local node = graph[name]
        if i == 1 then
            assert(node.pred[1] == "entry")
            node.dom = Set {name}
        else
            node.dom = Set {}
            for _, name in ipairs(traversal) do
                node.dom:add(name)
            end
        end
    end
    -- Iteration
    local changed = true
    local num_iter = 1
    while changed do
        changed = false
        print(("Iteration %d"):format(num_iter))
        for i = 2, #traversal do
            local name = traversal[i]
            local node = graph[name]
            local new_dom = node.dom
            -- Intersect the dominator sets of all predecessors of B
            for _, pred in ipairs(node.pred) do
                new_dom = new_dom:intersect(graph[pred].dom)
            end
            -- Union resulting set with {B}
            new_dom = new_dom:union(Set {name})
            if new_dom ~= node.dom then
                print(("%3s: %s => %s"):format(name, node.dom, new_dom))
                changed = true
                node.dom = new_dom
            end
        end
        num_iter = num_iter + 1
    end
end

function immediate_dominators(graph)
    print("Determining immediate dominators")
    local function immediate_dominator(node)
        if node.idom ~= nil then return node.idom end
        local idom = node.dom:intersect(Set(node.pred))
        if #idom == 0 then
            -- The immediate dominator is not a direct predecessor
            for _, pred in ipairs(node.pred) do
                -- The immediate dominator is shared by at least one but not
                -- necessarily every predecessor
                idom = node.dom:intersect(immediate_dominator(graph[pred]))
                if #idom == 1 then break end
            end
        end
        assert(#idom == 1)
        -- Cache result
        node.idom = idom
        return idom
    end
    for name, node in pairs(graph) do
        -- Exclude the first basic block after entry
        if node.dom and #node.dom > 1 then
            -- Immediate dominators are stored as singleton sets
            print(("%s idom %s"):format(next(immediate_dominator(node)), name))
        end
    end
end

function back_edges(graph)
    print("Identifying back edges")
    for name, node in pairs(graph) do
        for _, succ in ipairs(node.succ or {}) do
            if node.dom and node.dom[succ] then
                print(("%s => %s"):format(name, succ))
            end
        end
    end
end
