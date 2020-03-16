require "dependence"

local Array = require "array"

local A = Array "A"
local B = Array "B"
local C = Array "C"

local n = 10

for i in range(0, n) do
    for j in range(0, n) do
        for k in range(0, n) do
            S"S1"  use(C[i][j])  def(C[i][j])  use(A[i][k])  use(B[k][j])
        end
    end
end

assert(next(A.deps) == nil)
assert(next(B.deps) == nil)
print(C.deps)

--[[
> lua test_dependence.lua
S1 anti S1, d = (0, 0, 0)
S1 flow S1, d = (0, 0, 1)
S1 output S1, d = (0, 0, 1)
--]]
