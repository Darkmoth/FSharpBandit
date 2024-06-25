# Experimental Design Optimizer

This F# program implements a Monte Carlo Tree Search (MCTS) algorithm for experimental design optimization. It uses a binary tree structure to represent and analyze experimental data, and employs the Upper Confidence Bound (UCB) algorithm for tree traversal.

## Table of Contents

- [Features](#features)
- [Components](#components)
- [Functions](#functions)
- [Usage](#usage)
- [Installation](#installation)
- [Contributing](#contributing)
- [License](#license)

## Features

- Monte Carlo Tree Search (MCTS) implementation
- Upper Confidence Bound (UCB) algorithm for tree traversal
- Random data generation for simulations
- Data compaction and averaging
- Integration of theoretical data points

## Components

### Custom Types

- `NewObservation` and `Observation`: Represent experimental data
- `TreeNode` and `Tree`: Used to build the search tree

### Helper Functions

- `optionAdd` and `optionDiv`: Safe operations on optional values
- `from`: String formatting

### Tree Operations

- `TreeBuilder`: Recursively constructs a binary tree from observations
- `PickNode`: Implements the UCB algorithm for tree traversal

### Data Processing

- `InitBuilder`: Generates random observations
- `ObsCompact`: Groups and averages observations
- `AddTheory`: Adds theoretical test levels to the dataset

## Functions

1. `from`
2. `optionAdd`
3. `optionDiv`
4. `InitBuilder`
5. `TreeBuilder`
6. `PickNode`
7. `ObsCompact`
8. `AddTheory`
9. `main`

## Usage

The main functionality of the program:

1. Generates initial data using `InitBuilder`
2. Compacts the data with `ObsCompact`
3. Adds theoretical data points with `AddTheory`
4. Prints the resulting sequence

## Installation

(Add installation instructions here)

## Contributing

(Add contribution guidelines here)

## License

(Add license information here)