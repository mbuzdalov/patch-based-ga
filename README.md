# Patch-Based Genetic Algorithms

This is a framework for experiments with genetic algorithms that store populations as a tree of patches.

Please do not make your code depend on this project yet - it has to mature a lot before that!

## Improving Time and Memory Efficiency of Genetic Algorithms by Storing Populations as Minimum Spanning Trees of Patches

This is the first paper that introduces this project and performs first experiments. The experiments are as follows.

### Runtime measurements for OneMax

Run `sbt` as follows: `sbt "runMain com.github.mbuzdalov.patchga.main.OneMaxWallClockTimeMeasurements <algo> <flavor> <n>"`, where:
- `<algo>`: an algorithm to run, one of: `RLS`, `(1+1)`, `(2+1)`, `(10+1)`, `(50+1)`.
- `<flavor>`: the way to work with populations, one of: `naive`, `incre`.
- `<n>`: the problem size, a positive integer.

### Runtime measurements for the knapsack problem

Run `sbt` as follows: `sbt "runMain com.github.mbuzdalov.patchga.main.KnapsackWallClockTimeMeasurements <algo> <flavor> <n> <budget>"`, where:
- `<algo>`: an algorithm to run, one of: `RLS`, `(1+1)`, `(2+1)`, `(10+1)`, `(50+1)`.
- `<flavor>`: the way to work with populations, one of: `naive`, `incre`.
- `<n>`: the problem size, a positive integer.
- `<budget>`: the computational budget in fitness evaluations, a positive integer.

### Knapsack diversity measurement approximation

Run `sbt` as follows: `sbt "runMain com.github.mbuzdalov.patchga.main.KnapsackDiversityMeasurements <n> <budget>"`, where:
- `<n>`: the problem size, a positive integer.
- `<budget>`: the computational budget in fitness evaluations, a positive integer.
