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
- `<budget>`: the maximum computational budget in fitness evaluations, a positive integer.

## Never-Forgetting Genetic Algorithms: A Promising Architecture for Theory and Practice

The project was created with an intention to support really large populations and algorithms like this one.
All experiments are to be run as follows:

`sbt "runMain com.github.mbuzdalov.patchga.main.DistinctSamplesToOptimality <descriptor.yaml>`

where the descriptor is written in a YAML-like format with the following contents as an example:

```yaml
- algorithms
  - rls
    - RLS
  - heavy-ollga
    - (1+(L,L)) GA
      - mutation-distance-beta: 2.5
      - crossover-distance-beta: 2.5
  - nfga-local
    - NFGA
      - first-parent-selection-beta: 2.5
      - mutation-distance-beta: 1.5
      - crossover-probability: 0.5
      - crossover-parent-minimum-distance-beta: 1.5
      - second-parent-selection-beta: 2.5
      - crossover-distance: symmetric-heavy(2.5)
  - nfga-flat
    - NFGA
      - first-parent-selection-beta: 1.5
      - mutation-distance-beta: 1.5
      - crossover-probability: 0.5
      - crossover-parent-minimum-distance-beta: 1.5
      - second-parent-selection-beta: 1.5
      - crossover-distance: uniform-distance
- problems
  - linear-16-low
    - Linear
      - 1: 14
      - 2: 2
    - allow
      - all
  - onemax-16384
    - OneMax
      - size: 16384
    - allow
      - rls
      - heavy-ollga
      - nfga-flat
- runtime
  - stack: 67108864
  - processors: 16
  - runs: 11
```
