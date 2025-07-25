# Compiler Potpourri

A potpourri of programs, scripts, and tools for a compilers course covering:
- Three-address code
- Basic blocks
- Control flow analysis
- Data flow analysis
- Static single assignment (SSA) form
- SSA-based analysis and optimizations
- Introduction to LLVM IR
- Data dependences
- Loop transformations
- Parallel loops and tasks in OpenMP
- List scheduling

## References and Resources

### Books
- [Engineering a Compiler](https://www.elsevier.com/books/engineering-a-compiler/cooper/978-0-12-815412-0)
- [Modern Compiler Implementation in ML](https://www.cs.princeton.edu/~appel/modern/ml)
- [Compilers: Principles, Techniques, and Tools](https://www.pearson.com/us/higher-education/program/Aho-Compilers-Principles-Techniques-and-Tools-2nd-Edition/PGM167067.html)
- [High-Performance Compilers for Parallel Computing](https://dl.acm.org/doi/10.5555/572937)
- [Optimizing Compilers for Modern Architectures](https://www.elsevier.com/books/optimizing-compilers-for-modern-architectures/allen/978-0-08-051324-9)
- [SSA-based Compiler Design](https://link.springer.com/book/9783030805142)

### Papers
- [Compiler Transformations for High-Performance Computing](http://dl.acm.org/citation.cfm?id=197406)
- [Advanced Compiler Optimizations for Supercomputers](http://dl.acm.org/citation.cfm?id=7904)
- [A Catalogue of Optimizing Transformations](https://www.clear.rice.edu/comp512/Lectures/Papers/1971-allen-catalog.pdf)
- [A Simple, Fast Dominance Algorithm](https://www.cs.rice.edu/~keith/Embed/dom.pdf)
- [Iterative Data-Flow Analysis, Revisited](https://scholarship.rice.edu/handle/1911/96324)
- [SSA is Functional Programming](https://www.cs.princeton.edu/~appel/papers/ssafun.pdf)
- [Simple Generation of Static Single-Assignment Form](https://link.springer.com/content/pdf/10.1007/3-540-46423-9_8.pdf)
- [Practical Improvements to the Construction and Destruction of Static Single Assignment Form](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.49.9683)
- [Translating Out of Static Single Assignment Form](https://link.springer.com/chapter/10.1007/3-540-48294-6_13)
- [Combining Analysis, Combining Optimizations](https://scholarship.rice.edu/handle/1911/16807)
- [From Quads to Graphs: An Intermediate Representation's Journey](https://www.researchgate.net/publication/2746343_From_Quads_to_Graphs_An_Intermediate_Representation's_Journey)
- [Intermediate Representations in Imperative Compilers: A Survey](https://dl.acm.org/doi/abs/10.1145/2480741.2480743)
- [Value Numbering](http://softlib.rice.edu/pub/CRPC-TRs/reports/CRPC-TR94517-S.pdf)
- [Constant Propagation with Conditional Branches](https://dl.acm.org/doi/10.1145/103135.103136)

### Articles
- [Directed Graph Traversal, Orderings, and Applications to Data-Flow Analysis](https://eli.thegreenplace.net/2015/directed-graph-traversal-orderings-and-applications-to-data-flow-analysis/)
- [Understanding Static Single Assignment Forms](https://blog.yossarian.net/2020/10/23/Understanding-static-single-assignment-forms)
- A New Backend for Cranelift: [Part 1](https://cfallin.org/blog/2020/09/18/cranelift-isel-1), [Part 2](https://cfallin.org/blog/2021/01/22/cranelift-isel-2), [Part 3](https://cfallin.org/blog/2021/03/15/cranelift-isel-3)
- [Implementing a Toy Optimizer](https://www.pypy.org/posts/2022/07/toy-optimizer.html)
- [Search-based Compiler Code Generation](https://jamey.thesharps.us/2017/06/19/search-based-compiler-code-generation)
- [E-Graphs and Equality Saturation](https://docs.rs/egg/0.9.0/egg/tutorials/_01_background/index.html)
- [Snapshot Testing for Compilers and Compiler-Like Things](https://www.cs.cornell.edu/~asampson/blog/turnt.html)
- [Critical Edge Splitting](https://nickdesaulniers.github.io/blog/2023/01/27/critical-edge-splitting)
- [CPS Soup (A Functional Intermediate Language)](https://wingolog.org/archives/2023/05/20/approaching-cps-soup)
- [A Gentle Introduction to LLVM IR](https://mcyoung.xyz/2023/08/01/llvm-ir)
- [A Catalog of Ways to Generate SSA](https://bernsteinbear.com/blog/ssa)
- [What I Talk About When I Talk About IRs](https://bernsteinbear.com/blog/irs)
- [The Problem of Register Allocation](https://langdev.stackexchange.com/a/4326)

### Courses
- Compiler Design (CMU): [Fall 2014](http://www.cs.cmu.edu/~fp/courses/15411-f14/schedule.html), [Fall 2018](https://www.cs.cmu.edu/~janh/courses/411/18/schedule.html), [Fall 2020](https://www.cs.cmu.edu/afs/cs/academic/class/15411-f20/www/schedule.html)
- Compiler Design (KAIST): [Spring 2020](https://github.com/kaist-cp/cs420), [Spring 2022](https://github.com/kaist-cp/cs420)
- Introduction to Compilers (Cornell): [Spring 2020](https://www.cs.cornell.edu/courses/cs4120/2020sp), [Spring 2022](https://www.cs.cornell.edu/courses/cs4120/2022sp)
- Advanced Compilers (Cornell): [Fall 2020](https://www.cs.cornell.edu/courses/cs6120/2020fa), [Spring 2022](https://www.cs.cornell.edu/courses/cs6120/2022sp), [Spring 2025](https://www.cs.cornell.edu/courses/cs6120/2025sp)
- Compiler Optimization (UC Berkeley): [Fall 2024](https://github.com/mwillsey/cs265)
- Static Program Analysis (UFMG): [Spring 2020](https://homepages.dcc.ufmg.br/~fernando/classes/dcc888), [Spring 2022](https://homepages.dcc.ufmg.br/~fernando/classes/dcc888)
