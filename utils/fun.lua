local fun = {}

function fun.map(f, t)
    local tt = {}
    for _, v in ipairs(t) do
        table.insert(tt, (f(v)))
    end
    return tt
end

function fun.map2(f, s, t)
    local tt = {}
    for i = 1, math.min(#s, #t) do
        table.insert(tt, (f(s[i], t[i])))
    end
    return tt
end

function fun.filter(p, t)
    local tt = {}
    for _, v in ipairs(t) do
        if p(v) == true then table.insert(tt, v) end
    end
    return tt
end

function fun.fold(f, a, t)
    for _, v in ipairs(t) do
        a = f(a, v)
    end
    return a
end

-- Some convenience functions for common operations
fun["+1"] = function (a) return a + 1 end
fun["-1"] = function (a) return a - 1 end
fun["+"] = function (a, b) return a + b end
fun["-"] = function (a, b) return a - b end
fun["*"] = function (a, b) return a * b end
fun["/"] = function (a, b) return a / b end
fun["%"] = function (a, b) return a % b end

return fun
