package.path = "../../utils/?.lua;" .. package.path

local Set = require "set"
local fun = require "fun"

local Dependence = {}

function Dependence.analyze(a, b)
    assert(#a.subscripts == #b.subscripts)

    local subscripts = fun.map2(fun.pair, a.subscripts, b.subscripts)

    local variables = fun.map(function (pair)
        return Set(pair[1]:variables()) + Set(pair[2]:variables())
    end, subscripts)

    result = {}

    for dim = 1, #subscripts do
        local class
        if #variables[dim] == 0 then class = "ZIV"
        elseif #variables[dim] == 1 then class = "SIV"
        else class = "MIV" end

        local coupled = false
        for other_dim = 1, #subscripts do
            if other_dim ~= dim then
                if #(variables[other_dim] * variables[dim]) > 0 then
                    coupled = true
                    break
                end
            end
        end

        result[#result+1] = {
            subscript = {a.subscripts[dim], b.subscripts[dim]},
            variables = variables[dim],
            class = class,
            coupled = coupled
        }
    end

    return result
end

return Dependence
