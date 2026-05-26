// RUN: mlir-opt %s --test-memref-dependence-check 2>&1 | awk '/remark: dependence.*=.*\[/' | FileCheck %s

// Example from:
// https://blog.kaving.me/blog/analysing-a-benchmark-on-llvm-riscv

func.func @test1(%x: memref<?xi32>, %y: memref<?xi32>, %n: index) {
  %c0 = arith.constant 0 : index
  %c1 = arith.constant 1 : index
  %c1000 = arith.constant 1000 : index

  affine.for %k = 0 to 1000 {
    affine.for %i = 0 to %n {
      %xi = affine.load %x[%n - 1 - %i] : memref<?xi32>   // 0
      %yi = affine.load %y[%n - 1 - %i] : memref<?xi32>   // 1
      %sum = arith.addi %yi, %xi : i32
      affine.store %sum, %y[%n - 1 - %i] : memref<?xi32>  // 2
    }
  }

  return
}

// Before loop interchange:
// CHECK-DAG: dependence from 1 to 2 at depth 1 = [1, 999][0, 0]
//            1 anti 2, direction (<, =), redundant
// CHECK-DAG: dependence from 2 to 1 at depth 1 = [1, 999][0, 0]
//            2 flow 1, direction (<, =)
// CHECK-DAG: dependence from 2 to 2 at depth 1 = [1, 999][0, 0]
//            2 output 2, direction (<, =)

func.func @test2(%x: memref<?xi32>, %y: memref<?xi32>, %n: index) {
  %c0 = arith.constant 0 : index
  %c1 = arith.constant 1 : index
  %c1000 = arith.constant 1000 : index

  affine.for %i = 0 to %n {
    affine.for %k = 0 to 1000 {
      %xi = affine.load %x[%n - 1 - %i] : memref<?xi32>   // 3
      %yi = affine.load %y[%n - 1 - %i] : memref<?xi32>   // 4
      %sum = arith.addi %yi, %xi : i32
      affine.store %sum, %y[%n - 1 - %i] : memref<?xi32>  // 5
    }
  }

  return
}

// After loop interchange:
// CHECK-DAG: dependence from 4 to 5 at depth 2 = [0, 0][1, 999]
//            4 anti 5, direction (=, <), redundant
// CHECK-DAG: dependence from 5 to 4 at depth 2 = [0, 0][1, 999]
//            5 flow 4, direction (=, <)
// CHECK-DAG: dependence from 5 to 5 at depth 2 = [0, 0][1, 999]
//            5 output 5, direction (=, <)
