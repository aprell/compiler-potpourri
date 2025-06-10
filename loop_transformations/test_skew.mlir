// RUN: mlir-opt %s

func.func @test(%n: index, %m: index) {
    %c1 = arith.constant 1 : index
    %n1 = arith.addi %n, %c1 : index
    %m1 = arith.addi %m, %c1 : index
    affine.for %i = 1 to %n1 {
        affine.for %j = 1 to %m1 {
            // ...
        }
    }
    return
}

#inner_lo = affine_map<(is)[s0] -> (is + 1)>
#inner_hi = affine_map<(is)[s0] -> (is + s0)>
#i = affine_map<(is, js) -> (is)>
#j = affine_map<(is, js) -> (js - is)>

func.func @test_skewed(%n: index, %m: index) {
    %c1 = arith.constant 1 : index
    %n1 = arith.addi %n, %c1 : index
    %m1 = arith.addi %m, %c1 : index
    affine.for %is = 1 to %n1 {
        affine.for %js = #inner_lo(%is)[%m1] to #inner_hi(%is)[%m1] {
            %i = affine.apply #i(%is, %js)
            %j = affine.apply #j(%is, %js)
            // ...
        }
    }
    return
}
