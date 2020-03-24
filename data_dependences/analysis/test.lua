package.path = "../../utils/?.lua;" .. package.path

local Set = require "set"

local ArrayAccess = require "array_access"
local Dependence = require "dependence"

local function test(t)
    local result = Dependence.analyze(
        ArrayAccess(t.input[1]),
        ArrayAccess(t.input[2]))

    for dim, expect in ipairs(t.expect) do
        assert(result[dim].variables == expect.variables)
        assert(result[dim].class == expect.class)
        assert(result[dim].coupled == expect.coupled)
    end

    assert(Dependence.test(result) == t.dependence)
end

-- Separable subscripts

test {
    input = {
        "A[i]",
        "A[i-1]"
    },
    expect = {
        {
            variables = Set {"i"},
            class = "SIV",
            coupled = false
        }
    },
    dependence = true
}

test {
    input = {
        "A[2*i + 2]",
        "A[2*i + 1]"
    },
    expect = {
        {
            variables = Set {"i"},
            class = "SIV",
            coupled = false
        }
    },
    dependence = false
}

test {
    input = {
        "A[3*i]",
        "A[2*i + 1]"
    },
    expect = {
        {
            variables = Set {"i"},
            class = "SIV",
            coupled = false
        }
    },
    dependence = true
}

test {
    input = {
        "A[3*i][2*j - 4*k + 1]",
        "A[2*i + 1][2*j + 4*k - 6]"
    },
    expect = {
        {
            variables = Set {"i"},
            class = "SIV",
            coupled = false
        },
        {
            variables = Set {"j", "k"},
            class = "MIV",
            coupled = false
        }
    },
    dependence = false
}

test {
    input = {
        "A[5][i + 1][j]",
        "A[10][i][k]"
    },
    expect = {
        {
            variables = Set {},
            class = "ZIV",
            coupled = false
        },
        {
            variables = Set {"i"},
            class = "SIV",
            coupled = false
        },
        {
            variables = Set {"j", "k"},
            class = "MIV",
            coupled = false
        }
    },
    dependence = false
}

test {
    input = {
        "A[i + 1][j + 2][k + 3]",
        "A[i][j][k]"
    },
    expect = {
        {
            variables = Set {"i"},
            class = "SIV",
            coupled = false
        },
        {
            variables = Set {"j"},
            class = "SIV",
            coupled = false
        },
        {
            variables = Set {"k"},
            class = "SIV",
            coupled = false
        }
    },
    dependence = true
}

-- Coupled subscripts

test {
    input = {
        "A[i][j][j]",
        "A[i][j][k + i]"
    },
    expect = {
        {
            variables = Set {"i"},
            class = "SIV",
            coupled = true
        },
        {
            variables = Set {"j"},
            class = "SIV",
            coupled = true
        },
        {
            variables = Set {"i", "j", "k"},
            class = "MIV",
            coupled = true
        }
    },
    dependence = true
}
