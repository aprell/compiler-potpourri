require "dep"

local a = array "a"
local b = array "b"

for i in range(1, 16) do
    S"S1"  def(a[i+3])  use(a[i])  use(b[i])
end

print(a.deps)
assert(next(b.deps) == nil)

--[[
> lua strip_mining.lua
S1 flow S1, d = (3)
--]]

print "--- After strip mining ----------"

a = array "a"
b = array "b"

-- See ../loop_transformations/test_strip_mine.lua
for is in range(1, 16, 5) do
    for i in range(is, math.min(16, is + 4)) do
        S"S1"  def(a[i+3])  use(a[i])  use(b[i])
    end
end

print(a.deps)
assert(next(b.deps) == nil)

--[[
> lua strip_mining.lua
S1 flow S1, d = (0, 3)
S1 flow S1, d = (5, 3)

> lua strip_mining.lua --normalize
S1 flow S1, d = (0, 3)
S1 flow S1, d = (1, -2)
--]]
