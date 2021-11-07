-- RUN: lua %s --normalize | FileCheck %s
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

-- CHECK: S1 flow S1, d = (3)

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

-- COM: CHECK-DAG: S1 flow S1, d = (0, 3)
-- COM: CHECK-DAG: S1 flow S1, d = (5, 3)

-- CHECK-DAG: S1 flow S1, d = (0, 3)
-- CHECK-DAG: S1 flow S1, d = (1, -2)
