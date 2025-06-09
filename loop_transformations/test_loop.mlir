// RUN: mlir-opt %s --lower-affine

func.func @test(%n: index) {
    %c1 = arith.constant 1 : index
    %ub = arith.addi %n, %c1 : index
    affine.for %i = 1 to %ub {
        affine.for %j = 1 to %ub {
            affine.for %k = 1 to %ub {
                // ...
            }
        }
    }
    return
}
