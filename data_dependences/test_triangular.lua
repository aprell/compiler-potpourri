-- RUN: lua %s --normalize | FileCheck %s
require "dependence"
require "range"

local Array = require "array"

local A = Array "A"

for i in range(1, 100) do
    for j in range(i, 100) do
        S"S1"  def(A[i][j+1])  use(A[100][j])
    end
end

assert(next(A.deps) == nil)

local B = Array "B"

for i in range(1, 100) do
    for j in range(i, 100) do
        S"S1"  def(B[i][j+1])  use(B[99][j])
    end
end

print(B.deps)

-- CHECK-DAG: S1 flow S1, d = (0, 1)
-- CHECK-DAG: S1 flow S1, d = (1, 0)
-- CHECK-DAG: S1 anti S1, d = (1, -2)
