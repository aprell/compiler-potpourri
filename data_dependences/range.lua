local loop = require "loop"

-- Produce a sequence of integers from lo to hi (both inclusive) by step
function range(lo, hi, step)
    step = step or 1 -- Default step size
    assert(step ~= 0, "step must be different from zero")
    loop.push {lo = lo, hi = hi, step = step}
    if step > 0 then
        return function ()
            local i = lo
            if lo <= hi then
                lo = lo + step
                return i
            else
                loop.pop()
            end
        end
    else -- step < 0
        return function ()
            local i = lo
            if lo >= hi then
                lo = lo + step
                return i
            else
                loop.pop()
            end
        end
    end
end
