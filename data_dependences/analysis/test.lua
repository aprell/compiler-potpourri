local ArrayAccess = require "array_access"
local Dependence = require "dependence"

local result = Dependence.analyze(
    ArrayAccess "A[i + 1][j + 2][k + 3]",
    ArrayAccess "A[i][j][k]")

for i, s in ipairs(result) do
    print(i, s.subscript[1], s.subscript[2], s.variables, s.class, s.coupled)
end

local result = Dependence.analyze(
    ArrayAccess "A[5][i + 1][j]",
    ArrayAccess "A[10][i][k]")

for i, s in ipairs(result) do
    print(i, s.subscript[1], s.subscript[2], s.variables, s.class, s.coupled)
end

local result = Dependence.analyze(
    ArrayAccess "A[i][j][j]",
    ArrayAccess "A[i][j][k]")

for i, s in ipairs(result) do
    print(i, s.subscript[1], s.subscript[2], s.variables, s.class, s.coupled)
end

local result = Dependence.analyze(
    ArrayAccess "A[2*i + 2]",
    ArrayAccess "A[2*i + 1]")

for i, s in ipairs(result) do
    print(i, s.subscript[1], s.subscript[2], s.variables, s.class, s.coupled)
end

local result = Dependence.analyze(
    ArrayAccess "A[3*i]",
    ArrayAccess "A[2*i + 1]")

for i, s in ipairs(result) do
    print(i, s.subscript[1], s.subscript[2], s.variables, s.class, s.coupled)
end
