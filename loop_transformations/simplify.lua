local function simplify(expr)
    local ok, val = pcall(load("return " .. expr))
    return ok and val or expr
end

return simplify
