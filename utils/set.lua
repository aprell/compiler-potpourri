local Set = {}

local function from_list(lst)
    local set = {}
    for _, v in ipairs(lst) do
        set[v] = true
    end
    return set
end

local function to_list(set)
    local lst = {}
    for k, _ in pairs(set) do
        lst[#lst+1] = k
    end
    return lst
end

local function to_string(set)
    return "{" .. table.concat(to_list(set), ", ") .. "}"
end

local function add(set, e)
    set[e] = true
end

local function union(a, b)
    local set = Set {}
    for k in pairs(a) do
        set[k] = true
    end
    for k in pairs(b) do
        set[k] = true
    end
    return set
end

local function intersection(a, b)
    local set = Set {}
    for k in pairs(a) do
        if b[k] then set[k] = true end
        --set[k] = b[k]
    end
    return set
end

local function difference(a, b)
    local set = Set {}
    for k in pairs(a) do
        if not b[k] then set[k] = true end
    end
    return set
end

local function subset(a, b)
    for k in pairs(a) do
        if not b[k] then return false end
    end
    return true
end

local function proper_subset(a, b)
    return a <= b and not (b <= a)
end

local function equality(a, b)
    return a <= b and b <= a
end

local obj_mt = {
    __index = Set,
    __tostring = to_string,
    __add = union,
    __mul = intersection,
    __sub = difference,
    __le = subset,
    __lt = proper_subset,
    __eq = equality,
}

local class_mt = {
    __call = function (_, lst)
        local set = from_list(lst)
        return setmetatable(set, obj_mt)
    end
}

Set.add = add
Set.union = union
Set.intersect = intersection
Set.diff = difference

setmetatable(Set, class_mt)

--[[
local s1 = Set {1, 2, 3}
local s2 = Set {2, 1, 4}
print(s1,  s2)
print(s1 + s2)
print(s1 * s2)
print(s1 - s2)
assert(not (s1 <= s2))
assert(s1 * s2 <= s1)
assert(s1 < s1 + s2)
assert(s1 - s2 == Set {3})
--]]

return Set
