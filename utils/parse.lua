local parse = require "lpeg"

parse.alpha = parse.R ("AZ", "az")

parse.num = parse.R "09"

parse.alphanum = parse.alpha + parse.num

parse.space = parse.S " \t\n"

function parse.literal(s)
    return parse.space ^ 0 * parse.P (s) * parse.space ^ 0
end

parse.number = parse.C (
    parse.S "+-" ^ -1 * parse.num ^ 1
) / tonumber

parse.variable = parse.C (
    (parse.P "_" + parse.alpha) ^ 1 * (parse.P "_" + parse.alphanum) ^ 0
)

return parse
