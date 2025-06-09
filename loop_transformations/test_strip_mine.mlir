// RUN: mlir-opt %s --affine-loop-tile="tile-size=5" | FileCheck %s

func.func @test() {
    affine.for %i = 1 to 17 {
        // ...
    }
    return
}

// CHECK-DAG: [[$LB:#map[0-9]*]] = affine_map<(d0) -> (d0)>
// CHECK-DAG: [[$UB:#map[0-9]*]] = affine_map<(d0) -> (d0 + 5, 17)>

// CHECK:      affine.for %[[IS:.*]] = 1 to 17 step 5 {
// CHECK-NEXT:     affine.for %[[I:.*]] = [[$LB]](%[[IS]]) to min [[$UB]](%[[IS]]) {
//                     ...
// CHECK-NEXT:     }
// CHECK-NEXT: }
