--+----------------+
--| Strided ranges |
--+----------------+

local StridedRange = {}

local obj_mt = {
    __index = StridedRange,

    __tostring = function (range)
        return ("[%d%s:%d]%s"):format(
            range.lower_bound,
            range.step ~= 1 and ":" .. range.step or "",
            range.upper_bound,
            range.offset ~= nil and " + " .. tostring(range.offset) or "")
    end,

    __len = function(range)
        -- ((range.upper_bound - range.lower_bound + 1) + (range.step - 1)) / range.step
        return math.floor((range.upper_bound - range.lower_bound + range.step) / range.step)
    end
}

local class_mt = {
    __call = function (_, range)
        range.step = range.step or 1
        return setmetatable(range, obj_mt)
    end
}

setmetatable(StridedRange, class_mt)

-- Normalize range without changing the step size
function StridedRange:normalize()
    self.offset = self.lower_bound
    self.upper_bound = self.upper_bound - self.lower_bound
    self.lower_bound = 0
    return self
end

local a = StridedRange {
	lower_bound = 0, upper_bound = 10, step = 2
}

local b = StridedRange {
	lower_bound = 1, upper_bound = 10
}

local c = StridedRange {
	lower_bound = 3, upper_bound = 12, step = 4
} : normalize()

assert(tostring(a) == "[0:2:10]")
assert(tostring(b) == "[1:10]")
assert(tostring(c) == "[0:4:9] + 3")

return StridedRange
