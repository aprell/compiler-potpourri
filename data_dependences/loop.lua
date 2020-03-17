local loop = {}

-- Loops in scope
loop.loops = {}

-- Enter loop l
function loop.push(l)
    return table.insert(loop.loops, l)
end

-- Leave current loop
function loop.pop()
    return table.remove(loop.loops)
end

return loop
