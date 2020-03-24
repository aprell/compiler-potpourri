require "dependence"
require "range"

local Array = require "array"

local A = Array "A"
local B = Array "B"

for i in range(1, 16) do
    S"S1"  def(A[i+3])  use(A[i])  use(B[i])
end

print(A.deps)
assert(next(B.deps) == nil)

--[[
> lua test_strip_mining.lua
S1 flow S1, d = (3)
--]]

print "--- After strip mining ----------"

A = Array "A"
B = Array "B"

-- See ../loop_transformations/test_strip_mine.lua
for is in range(1, 16, 5) do
    for i in range(is, math.min(16, is + 4)) do
        S"S1"  def(A[i+3])  use(A[i])  use(B[i])
    end
end

print(A.deps)
assert(next(B.deps) == nil)

--[[
> lua test_strip_mining.lua
S1 flow S1, d = (0, 3)
S1 flow S1, d = (5, 3)

> lua test_strip_mining.lua --normalize
S1 flow S1, d = (0, 3)
S1 flow S1, d = (1, -2)
--]]
