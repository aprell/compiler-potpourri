-- RUN: lua %s | FileCheck %s
require "dependence"
require "range"

local Array = require "array"

local x = Array "x"
local y = Array "y"

local n = 10

-- Example from:
-- https://blog.kaving.me/blog/analysing-a-benchmark-on-llvm-riscv

for k in range(0, 1000) do
    for i in range(n-1, 0, -1) do
        S"S1"  def(y[i])  use(x[i])
    end
end

print("Before loop interchange: " .. tostring(y.deps))
-- CHECK: S1 output S1, d = (1, 0)

local function clear(...)
    for _, a in ipairs({...}) do
        for k in pairs(a) do
            a[k] = nil
        end
    end
end

clear(x.array, x.deps)
clear(y.array, y.deps)

for i in range(n-1, 0, -1) do
    for k in range(0, 1000) do
        S"S1"  def(y[i])  use(x[i])
    end
end

print("After loop interchange: " .. tostring(y.deps))
-- CHECK: S1 output S1, d = (0, 1)
