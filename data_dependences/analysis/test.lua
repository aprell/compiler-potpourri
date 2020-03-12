local ArrayAccess = require "array_access"
local Dependence = require "dependence"

local function test(t)
    local result = Dependence.analyze(
        ArrayAccess(t.input[1]),
        ArrayAccess(t.input[2]))

    for i, s in ipairs(result) do
        print(i, s.subscript[1], s.subscript[2], s.variables, s.class, s.coupled)
    end

    assert(Dependence.test(result) == t.expect)
end

test {
    input = {
        "A[i + 1][j + 2][k + 3]",
        "A[i][j][k]"
    },
    expect = true
}

test {
    input = {
        "A[5][i + 1][j]",
        "A[10][i][k]"
    },
    expect = false
}

test {
    input = {
        "A[i][j][j]",
        "A[i][j][k]"
    },
    expect = true
}

test {
    input = {
        "A[2*i + 2]",
        "A[2*i + 1]"
    },
    expect = false
}

test {
    input = {
        "A[3*i]",
        "A[2*i + 1]"
    },
    expect = true
}
