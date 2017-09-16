require "dep"

local A = array "A"
local B = array "B"
local C = array "C"

local n = 10

for i = 0, n do
    for j = 0, n do
        for k = 0, n do
            S"S1"  use(C[i][j])  def(C[i][j])  use(A[i][k])  use(B[k][j])
        end
    end
end

--[[
> lua test_dep.lua | sort | uniq
S1 anti S1, d = (0, 0, 0)
S1 flow S1, d = (0, 0, 1)
S1 output S1, d = (0, 0, 1)
--]]
