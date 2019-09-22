### Papers for the course Program Semantics & Verification 19/20

1. Cadar, C., & Sen, K. (2013). _Symbolic execution for software testing: three decades later_. Commun. ACM, 56(2), 82-90.

   Tags: symbolic.

   **Abstract.** Recent years have witnessed a surge of interest in symbolic execution for software testing, due to its ability to generate high-coverage test suites and find deep errors in complex software applications. In this article, we give an overview of modern symbolic execution techniques, discuss their key challenges in terms of path exploration, constraint solving, and memory modeling, and discuss several solutions drawn primarily from the authors’ own work.

1. Leino, K. R. M. (2005). _Efficient weakest preconditions_. Information Processing Letters, 93(6), 281-288.

   Tags: symbolic.

   **Abstract.** Desired computer-program properties can be described by logical formulas called verification conditions. Different mathematically-equivalent forms of these verification conditions can have a great impact on the performance of an automatic theorem prover that tries to discharge them. This paper presents a simple weakest-precondition understanding of the ESC/Java technique for generating verification conditions. The new understanding of this technique spotlights the program property that makes the technique work.    

1. Biere, A., Cimatti, A., Clarke, E. M., Strichman, O., & Zhu, Y. (2003). _Bounded model checking_. Advances in computers, 58(11), 117-148.

    Tags: SAT based BMC

    **Abstract.** . Symbolic model checking with Binary Decision Diagrams (BDDs) has been successfully used in the last decade for formally verifying finite state systems such as sequential circuits and protocols. Since its introduction in the beginning of the 90’s, it has been integrated in the quality assurance process of several major hardware companies. The main bottleneck of this method is that BDDs may grow exponentially, and hence the amount of available memory restricts the size of circuits that can be verified efficiently. In this article we survey a technique called Bounded Model Checking (BMC), which uses a propositional SAT solver rather than BDD manipulation techniques. Since its introduction in 1999, BMC has been well received by the industry. It can find many logical errors in complex systems that can not be handled by competing techniques, and is therefore widely perceived as a complementary technique to BDD-based model checking. This observation is supported by several independent comparisons that have been published in the last few years.

1. De Moura, L., & Bjørner, N. (2011). _Satisfiability modulo theories: introduction and applications_. Communications of the ACM, 54(9), 69-77.

    Tags: backend theorem proving, SMT.

    **Abstract.** there is no abstract :|, but it is a good introductory article to the subject.

1. Cavada et al. (2014, July). _The nuXmv symbolic model checker_. In International Conference on Computer Aided Verification. Springer.

    Tags: MC.

    **Abstract.** This paper describes the NUXMV symbolic model checker for finiteand infinite-state synchronous transition systems. NUXMV is the evolution of the NUSMV open source model checker. It builds on and extends NUSMV along two main directions. For finite-state systems it complements the basic verification techniques of NUSMV with state-of-the-art verification algorithms. For infinitestate systems, it extends the NUSMV language with new data types, namely Integers and Reals, and it provides advanced SMT-based model checking techniques. Besides extended functionalities, NUXMV has been optimized in terms of performance to be competitive with the state of the art. NUXMV has been used in several industrial projects as verification back-end, and it is the basis for several extensions to cope with requirements analysis, contract based design, model checking of hybrid systems, safety assessment, and software model checking.

1. Baldoni, R., Coppa, E., D’elia, D. C., Demetrescu, C., & Finocchi, I. (2018). _A survey of symbolic execution techniques_. ACM Computing Surveys (CSUR), 51(3), 50.

    Tags: symbolic.

    **Abstract.** Many security and software testing applications require checking whether certain properties of a program hold for any possible usage scenario. For instance, a tool for identifying software vulnerabilities may need to rule out the existence of any backdoor to bypass a program’s authentication. One approach would be to test the program using different, possibly random inputs. As the backdoor may only be hit for very specific program workloads, automated exploration of the space of possible inputs is of the essence. Symbolic execution provides an elegant solution to the problem, by systematically exploring many possible execution paths at the same time without necessarily requiring concrete inputs. Rather than taking on fully specified input values, the technique abstractly represents them as symbols, resorting to constraint solvers to construct actual instances that would cause property violations. Symbolic execution has been incubated in dozens of tools developed over the last four decades, leading to major practical breakthroughs in a number of prominent software reliability applications. The goal of this survey is to provide an overview of the main ideas, challenges, and solutions developed in the area, distilling them for a broad audience.

1. Baranová et al. (2017, October). _Model checking of C and C++ with DIVINE 4_. In International Symposium on Automated Technology for Verification and Analysis. Springer.

   Tags: explicit state MC.

   **Abstract.** The fourth version of the DIVINE model checker provides a modular platform for verification of real-world programs. It is built around an efficient interpreter of LLVM code which, together with a small, verification-oriented operating system and a set of runtime libraries, en- ables verification of code written in C and C++.

1. Havel, V. (2014). _Generic Platform for Explicit-Symbolic Verification_ (Doctoral dissertation, Masarykova univerzita, Fakulta informatiky).

   Tags: symbolic.

   **Abstract.** In this thesis we present a new generic platform SymDivine that may serve as a basis for a program analysis tool that can handle both single-threaded and multi-threaded programs written in the LLVM intermediate language. SymDivine implements the so-called control explicit-data symbolic approach in order to cope with programs with both nontrivial thread interleaving and large input domains. We experimentally evaluate a symbolic execution-like algorithm implemented on the top of SymDivine on a large set of bench- marks and compare it with CPAchecker.

1. Clarke, E., Kroening, D., & Lerda, F. (2004, March). _A tool for checking ANSI-C programs_. In International Conference on Tools and Algorithms for the Construction and Analysis of Systems. Springer.

   Tags: symbolic.

   **Abstract.** We present a tool for the formal verification of ANSI-C programs using Bounded Model Checking (BMC). The emphasis is on usability: the tool supports almost all ANSI-C language features, including pointer constructs, dynamic memory allocation, recursion, and the float and double data types. From the perspective of the user, the verification is highly automated: the only input required is the BMC bound. The tool is integrated into a graphical user interface. This is essential for presenting long counterexample traces: the tool allows stepping through the trace in the same way a debugger allows stepping through a program.

1. Cordeiro, L., Kesseli, P., Kroening, D., Schrammel, P., & Trtik, M. (2018, July). _JBMC: A bounded model checking tool for verifying Java bytecode_. In International Conference on Computer Aided Verification. Springer.

     Tags: symbolic, OO.

     **Abstract.** We present a bounded model checking tool for verifying Java bytecode, which is built on top of the CPROVER framework, named Java Bounded Model Checker (JBMC). JBMC processes Java bytecode together with a model of the standard Java libraries and checks a set of desired properties. Experimental results show that JBMC can correctly verify a set of Java benchmarks from the literature and that it is competitive with two state-of-the-art Java verifiers.

1. Barnett, M., Chang, B. Y. E., DeLine, R., Jacobs, B., & Leino, K. R. M. (2005, November). _Boogie: A modular reusable verifier for object-oriented programs_. In International Symposium on Formal Methods for Components and Objects. Springer.   

    Tags: symbolic, OO.

    **Abstract.** A program verifier is a complex system that uses compiler technology, program semantics, property inference, verification-condition generation, automatic decision procedures, and a user interface. This paper describes the architecture of a state-of-the-art program verifier for object-oriented programs.

1. Toman, J., Pernsteiner, S., & Torlak, E. (2015, November). _CRUST: A bounded verifier for Rust_. In 2015 30th IEEE/ACM International Conference on Automated Software Engineering (ASE). IEEE.

    Tags: symbolic.

    **Abstract.** Rust is a modern systems language that provides guaranteed memory safety through static analysis. However, Rust includes an escape hatch in the form of “unsafe code,” which the compiler assumes to be memory safe and to preserve crucial pointer aliasing invariants. Unsafe code appears in many data structure implementations and other essential libraries, and bugs in this code can lead to memory safety violations in parts of the program that the compiler otherwise proved safe. We present CRUST, a tool combining exhaustive test generation and bounded model checking to detect memory safety errors, as well as violations of Rust’s pointer aliasing invariants within unsafe library code. CRUST requires no programmer annotations, only an indication of the modules to check. We evaluate CRUST on data structures from the Rust standard library. It detects memory safety bugs that arose during the library’s development and remained undetected for several months.

1. Kant, G., Laarman, A., Meijer, J., van de Pol, J., Blom, S., & van Dijk, T. (2015, April). _LTSmin: high-performance language-independent model checking_. In International Conference on Tools and Algorithms for the Construction and Analysis of Systems. Springer.

    Tags: MC.

    **Abstract.** Inrecentyears,theLTSminmodelcheckerhasbeenextended with support for several new modelling languages, including probabilis- tic (Mapa) and timed systems (Uppaal). Also, connecting additional language front-ends or ad-hoc state-space generators to LTSmin was sim- plified using custom C-code. From symbolic and distributed reachability analysis and minimisation, LTSmin’s functionality has developed into a model checker with multi-core algorithms for on-the-fly LTL checking with partial-order reduction, and multi-core symbolic checking for the modal μ- calculus, based on the multi-core decision diagram package Sylvan.

    In LTSmin, the modelling languages and the model checking algo- rithms are connected through a Partitioned Next-State Interface (Pins), that allows to abstract away from language details in the implementation of the analysis algorithms and on-the-fly optimisations. In the current pa- per, we present an overview of the toolset and its recent changes, and we demonstrate its performance and versatility in two case studies.

1. Kahsai, T., Navas, J. A., Gurfinkel, A., & Komuravelli, A. (2015). _The SeaHorn verification framework_. In Computer Aided Verification (Vol. 205).

    Tags: symbolic, LLVM.

    **Abstract.** In this paper, we present SeaHorn, a software verification framework. The key distinguishing feature of SeaHorn is its modular design that separates the concerns of the syntax of the programming language, its operational semantics, and the verification semantics. SeaHorn encompasses several novelties: it (a) encodes verification conditions using an efficient yet precise inter-procedural technique, (b) provides flexibility in the verification semantics to allow different levels of precision, (c) leverages the state-of-the-art in software model checking and abstract interpretation for verification, and (d) uses Horn-clauses as an intermediate language to represent verification conditions which simplifies interfacing with multiple verification tools based on Horn-clauses. SeaHorn provides users with a powerful verification tool and researchers with an extensible and customizable framework for experimenting with new software verification techniques. The effectiveness and scalability of SeaHorn are demonstrated by an extensive experimental evaluation using benchmarks from SV-COMP 2015 and real avionics code.

1. Siegel et al. (2015, November). _CIVL: the concurrency intermediate verification language_. In SC'15: Proceedings of the International Conference for High Performance Computing, Networking, Storage and Analysis. IEEE.

    Tags: symbolic, concurrency.

    **Abstract** There are many ways to express parallel programs: message- passing libraries (MPI) and multithreading/GPU language extensions such as OpenMP, Pthreads, and CUDA, are but a few. This multitude creates a serious challenge for developers of software verification tools: it takes enormous effort to develop such tools, but each development effort typically targets one small part of the concurrency landscape, with little sharing of techniques and code among efforts. To address this problem, we present CIVL: the Concurrency Intermediate Verification Language. CIVL provides a general concurrency model capable of representing programs in a variety of concurrency dialects, including those listed above. The CIVL framework currently includes front-ends for the four dialects, and a back-end verifier which uses model checking and symbolic execution to check a number of properties, including the absence of deadlocks, race conditions, assertion violations, illegal pointer dereferences and arithmetic, memory leaks, divisions by zero, and out-of- bound array indexing; it can also check that two programs are functionally equivalent.

1. Khurshid, S., Păsăreanu, C. S., & Visser, W. (2003, April). _Generalized symbolic execution for model checking and testing_. In International Conference on Tools and Algorithms for the Construction and Analysis of Systems. Springer.

    Tags: hybrid concrete - symbolic, concurrency.

    **Abstract.** Modern software systems, which often are concurrent and manipulate complex data structures must be extremely reliable. We present a novel framework based on symbolic execution, for automated checking of such systems. We provide a two-fold generalization of traditional symbolic execution based approaches. First, we define a source to source translation to instrument a program, which enables standard model checkers to perform symbolic execution of the program. Second, we give a novel symbolic execution algorithm that handles dynamically allocated structures (e.g., lists and trees), method preconditions (e.g., acyclicity), data (e.g., integers and strings) and concurrency. The program instrumentation enables a model checker to automatically explore different program heap configurations and manipulate logical formulae on program data (using a decision procedure). We illustrate two applications of our framework: checking correctness of multi-threaded programs that take inputs from unbounded domains with complex structure and generation of non-isomorphic test inputs that satisfy a testing criterion. Our implementation for Java uses the Java PathFinder model checker.

1. Stolz, V., & Huch, F. (2005). _Runtime verification of concurrent Haskell programs_. Electronic Notes in Theoretical Computer Science, 113, 201-216.

    Tags: explicit state RTV, concurrency, functional.

    **Abstract.** In this article we use model checking techniques to debug Concurrent Haskell programs. LTL formulas specifying assertions or other properties are verified at runtime. If a run which falsifies a formula is detected, the debugger emits a warning and records the path leading to the violation. It is possible to dynamically add formulas at runtime, giving a degree of flexibility which is not available in static verification of source code. We give a comprehensive example of using the new techniques to detect lock-reversal in Concurrent Haskell programs and introduce a template mechanism to define LTL formulas ranging over an arbitrary set of threads or communication abstractions.
