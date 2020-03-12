local Array = {}

local obj_mt = {
    __index = function (a, i)
        table.insert(a.last_ref, i)
        return a
    end
}

local class_mt = {
    __call = function (_, name)
        local a = {name = name, array = {}, last_ref = {}}
        a.deps = setmetatable({}, {
            __tostring = function (deps)
                local t = {}
                for dep in pairs(deps) do
                    t[#t+1] = dep
                end
                return table.concat(t, "\n")
            end
        })
        return setmetatable(a, obj_mt)
    end
}

setmetatable(Array, class_mt)

return Array
