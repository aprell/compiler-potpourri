// RUN: mlir-opt %s --affine-loop-tile="tile-size=10" | FileCheck %s

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

// CHECK-DAG: [[$LB:#map[0-9]*]] = affine_map<(d0) -> (d0)>
// CHECK-DAG: [[$UB:#map[0-9]*]] = affine_map<(d0)[s0] -> (d0 + 10, s0)>

// CHECK:      %[[C1:.*]] = arith.constant 1 : index
// CHECK-NEXT: %[[N1:.*]] = arith.addi %{{.*}}, %[[C1]] : index
// CHECK-NEXT: %[[M1:.*]] = arith.addi %{{.*}}, %[[C1]] : index
// CHECK-NEXT: affine.for %[[IT:.*]] = 1 to %[[N1]] step 10 {
// CHECK-NEXT:     affine.for %[[JT:.*]] = 1 to %[[M1]] step 10 {
// CHECK-NEXT:         affine.for %[[I:.*]] = [[$LB]](%[[IT]]) to min [[$UB]](%[[IT]])[%[[N1]]] {
// CHECK-NEXT:             affine.for %[[J:.*]] = [[$LB]](%[[JT]]) to min [[$UB]](%[[JT]])[%[[M1]]] {
//                             ...
// CHECK-NEXT:             }
// CHECK-NEXT:         }
// CHECK-NEXT:     }
// CHECK-NEXT: }
