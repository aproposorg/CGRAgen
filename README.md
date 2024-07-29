# CGRAgen

CGRAgen is a generic CGRA architecture modeling, hardware generation, and application mapping flow that lets developers more easily perform design space exploration of CGRA architectures. Its key components are modeled after [CGRA-ME](https://cgra-me.ece.utoronto.ca/) version 1.0 but has several differences. The following provides a brief overview of CGRAgen's capabilities.

If you have used CGRAgen in a research project, please cite it as:
```bibtex
@inproceedings{damsgaard2023generating,
  title={{Generating CGRA Processing Element Hardware with CGRAgen}},
  author={Damsgaard, Hans Jakob and Ometov, Aleksandr and Nurmi, Jari},
  booktitle={26th Euromicro Conference on Digital System Design (DSD)},
  year={2023},
  pages={1--7},
  publisher={IEEE}
}
```

CGRAgen has been developed and tested on Ubuntu 22.04 (with Ubunty Java 11.0.22) and Windows (with Eclipse Adoptium Java 11.0.18 64-bit).

## How does it work?

CGRAgen integrates two main components: architecture and application modeling. These components are mostly separate but both are used when performing mapping. Its command line interface works as follows:

```bash
$ sbt 'runMain cgragen.CGRAgen [-h[elp]] [-debug] [-gen-hw] [-dataSize INT] [-ii INT] [-print-dfg] [-print-arch] [-infer-io] [-m[apper] MPR] [[-m-arg]/[--mapper-arg] K=V] [-remove-dead] [-prop-const] [-avoid-mc-const] [-limit-fanout [INT]] [-approx-arith] path/to/arch [path/to/dfg]'
```

The flags prefixed by one or two hyphens adjust the operation of the tool:

- `-h[elp]` prints the help message and exits the program
- `-debug` enables printing of all debug messages to `stdout`, making the tool rather verbose
- `-gen-hardware` enables the Chisel-based hardware generation backend
- `-dataSize INT` sets the default data path bit-width to `INT`
- `-ii INT` sets the maximum acceptable `II` in the mapper to `INT`
- `-print-dfg` enables printing of the parsed DFG in .dot format to a file
- `-print-arch` enables printing of the parsed architecture in .dot format to a file
- `-infer-io` enables inference of top-level IO of the CGRA as the unconnected ports of its sub-modules
- `-m[apper] MPR` sets the desired mapping engine to `MPR`
- `-m-arg` or `-mapper-arg` passes a key-value pair `K = V` to the mapping engine
- `-approx-arith` enables the use of inexact arithmetic globally in the CGRA
- `-remove-dead` enables removal of dead nodes in the DFG
- `-prop-const` enables propagation of constants in the DFG
- `-avoid-mc-const` enables replication of multi-cast constants in the DFG
- `-limit-fanout` enables duplication of all node types to limit maximum fanout in the DFG

CGRAgen runs only for hardware generation when the path to the DFG file is omitted. Inference of top-level IO is available only when no top-level ports are instantiated in a design. The inferred IO works both in mapping and hardware generation. CGRAgen ships with a [configuration file](./src/main/resources/cgragen.properties) that provides default values for all these settings. The tool generates a new folder under the `./runs` directory each time it is executed.

### Architectures

CGRA descriptions for CGRAgen use an adapted version of CGRA-ME version 1.0's XML-based ADL. It consists of three elements: (1) optional named constant definitions, (2) optional module template definitions, and (3) an architecture with patterns of module instantiations and interconnections. The description should be in a single file and follow the hierarchy shown below. The order of fields doesn't matter; definitions are parsed before module templates, which again are parsed before the architecture. The parser will error if unexpected fields are found outside their expected hierarchical positions. Fields and names are case-sensitive.

```xml
<CGRA>
    <definition name="%CONSTANT_NAME%" value="%CONSTANT_VALUE%"/> ...
    <template name="%TEMPLATE_NAME%"> 
        <input name="%INPUT_NAME%"/> ...
        <output name="%OUTPUT_NAME%"/> ...
        <inst name="%INSTANCE_NAME%" module="%PRIMITIVE_TYPE%"/> ...
        <submodule name="%INSTANCE_NAME%" module="%MODULE_TYPE%"/> ...
        <wire name="%WIRE_NAME%"/> ...
        <connection from="%FROM_CONNECTION%" to="%TO_CONNECTION%"/> ...
        <connection from="%FROM_CONNECTION%" distribute-to="%TO_CONNECTIONS%"/> ...
        <connection select-from="%FROM_CONNECTIONS%" to="%TO_CONNECTION%"/> ...
    </template> ...
    <architecture row="%ROWS%" col="%COLUMNS%">
        <pattern row-range="%FROM_ROW TO_ROW%" col-range="%FROM_COL TO_COL%"
                 row-counter="%NAME%" col-counter="%NAME%"
                 wrap-around="%0/1%" wrap-col="%0/1%" wrap-row="%0/1%">
            <block module="%MODULE_TYPE%"/> ...
            <connection from="%FROM_CONNECTION%" to="%TO_CONNECTION%"/> ...
        </pattern> ...
    </architecture>
</CGRA>
```

The basic elements of a CGRA that can be used in module template descriptions are shown above. They are:

- **Ports** defined by fields with the `<input>` or `<output>` tag. Ports are named and may have an optional width which otherwise defaults to [cgra.DataSize](../../../resources/cgragen.properties#L8). For simplicity, CGRAgen provides support only for uni-directional ports.
- **Wires** defined by fields with the `<wire>` tag. Wires act as intermediates for connections but are optimized away during conversion to CGRAgen's internal representation.
- **Instances** defined by fields with the `<inst>` tag. Instances refer to primitives whose types are determined by their `module` attributes.
- **Connections** defined by fields with the `<connection>` tag. Connections abstract links between ports, wires, and instances' ports. Depending on the connection needed, combinations of attributes determine how the connection is implemented. The currently supported combinations are:
  - `from` and `to` both taking a single port/wire name as arguments.
  - `select-from` and `to` taking a list of ports/wires and a single port/wire as arguments, respectively. This is useful for implementing multiplexed connections from multiple sources to a common sink.
  - `from` and `distribute-to` taking a single port/wire and a list of ports/wires as arguments, respectively. Useful for forking a source to multiple sinks.

The basic elements of an architecture are:

- **Blocks** defined by fields with the `<block>` tag. Blocks refers to instances of templated modules whose types are determined by their `module` attributes. Block instantiations should not be present outside `<pattern>` fields as, internally, they are named depending on their respective locations in the CGRA mesh.
- **Patterns** defined by fields with the `<pattern>` tag. Patterns are used to define regular patterns of block instances or connections between existing blocks. Only connections of type (`from`, `to`) and (`from`, `distribute-to`) are supported within patterns. However, flexibility is enabled with relative references using the `(rel x y)` notation, where `x` and `y` are the row, respectively, column offsets of the referenced block. Assigning the pattern the `wrap-col`, `wrap-row`, or `wrap-around` attributes lets these offsets operate modulo the number of columns, rows, or both columns and rows in the pattern. Three additional counters for rows, columns, and pattern iterations may also be specified and used in place of the offsets `x` and `y`.

Instances denoted by the `<inst>` refer to primitives whose types are determined by their `module` attributes. Currently supported primitives and their attributes are:

- `ConstUnit`: a "function" unit representing a constant value to be included in the CGRA configuration
  - `name` (mandatory)
  - `size` (optional, defaults to [cgra.DataSize](../../../resources/cgragen.properties#L8))
- `Multiplexer`: a simple multiplexer
  - `name` (mandatory)
  - `size` (optional, defaults to [cgra.DataSize](../../../resources/cgragen.properties#L8))
  - `ninput` (mandatory)
- `Register`: a simple positive edge-triggered register which is always enabled
  - `name` (mandatory)
  - `size` (optional, defaults to [cgra.DataSize](../../../resources/cgragen.properties#L8))
- `FuncUnit`: a (for now purely combinational) function unit
  - `name` (mandatory)
  - `size` (optional, defaults to [cgra.DataSize](../../../resources/cgragen.properties#L8))
  - `approx` (optional, defaults to [cgra.ApproximateArithmetic](../../../resources/cgragen.properties#L13))
  - `ops` (optional, defaults to [cgra.FuncUnit.Ops])
  - `IIs` (optional, should satisfy `len(IIs) == len(ops)` if provided, defaults to all [cgra.FuncUnit.II](../../../resources/cgragen.properties#L10))
  - `latencies` (optional, should satisfy `len(latencies) == len(ops)` if provided, defaults to all [cgra.FuncUnit.Latency](../../../resources/cgragen.properties#L11))
- `RegisterFile`: a simple register file with a parameterizable number of read and write ports
  - `name` (mandatory)
  - `size` (optional, defaults to [cgra.DataSize](../../../resources/cgragen.properties#L8))
  - `ninput` (mandatory)
  - `noutput` (mandatory)
  - `log2-nregister` (optional, defaults to [cgra.RegisterFile.Log2NRegister](../../../resources/cgragen.properties#L22))

Multiplexer primitives are automatically instantiated for connections with the `select-from` attribute.

CGRAgen's hardware generation engine is built with Chisel and constructs the modeled hardware bottom-up. This leads to some complexities, described in a previous [paper](https://ieeexplore.ieee.org/abstract/document/10456841) of ours, which are worked around quite neatly. Note that generating architectures with multiple inexact multipliers takes a long time due to their custom compressor trees built with native Chisel from our [approx library](https://github.com/aproposorg/approx).

### Applications

DFG descriptions for CGRAgen use a simple subset of the .dot language with any number of node and edge declarations. CGRAgen integrates a custom `RegexParser` that reads such descriptions and translates them into actual graphs. All nodes must define an `opcode` attribute that describes their arithmetic/logic operation. All edges must, in addition to a pair of source and sink nodes, also specify an `operand` attribute that indicates their arithmetic position in the sink operation. For instance, an addition node should be the sink of two edges with `operand` attributes 0 and 1. No two edges can share the same `operand` attribute in a sink node.

```dot
digraph %GRAPH_NAME% {
  %NODE_NAME%[%ATTR_NAME%=%ATTR_VAL% ...]; ...
  %SRC_NODE_NAME%->%SNK_NODE_NAME%[%ATTR_NAME%=%ATTR_VAL% ...]; ...
}
```

The optimizations to the DFG outlined above are implemented as optional compiler-like passes that may follow parsing and precede mapping.

## Installation

CGRAgen has relatively few dependencies and utilizes SBT for managing any Scala-related ones. To install SBT, please see the guide [here](https://www.scala-sbt.org/download.html).

When cloning the repository, make sure to initialize and clone its sub-modules too:

```bash
$ git clone https://github.com/aproposorg/cgragen.git
$ cd cgragen
$ git submodule init
$ git submodule update
```

Alternatively, download the sub-modules recursively when cloning the main repository:

```bash
$ git clone --recurse-submodules https://github.com/aproposorg/cgragen.git
```

## License

CGRAgen is provided under the MIT license. See [LICENSE](./LICENSE) for more details.

## Suggested extensions

Some ideas for where to extend CGRAgen follow below:

- Solid regression test system (based on the included Scalatests)
- Bit-width understanding between architecture and application
- Simple math and data size adjustments to template-based sub-modules
- Conditional constructs (like `<if> [<elsif>] <else>`) and/or loops (like `<for counter="%COUNTER_NAME%" range="%RANGE%">`) in the ADL
- Automatic verification of composite modules (inspired by the verification flow for primitives)
- Support for bus-style interconnects with multiple drivers (tri-state only possible with black-boxes, OR-based bus [works](https://scastie.scala-lang.org/fIrbSSGUQESfIS0gPdPZrg))
- Replace some custom exceptions with optional return types instead (and improve the error/help messages)
- Exploit the benefits of Chisel's [Hierarchy](https://www.chisel-lang.org/docs/cookbooks/hierarchy) features better than only in the top-level CGRA module
- Mitigate the DAG requirement in mapping and approximation flows

## Questions?

Feel free to reach out at hans (dot) damsgaard (at) tuni (dot) fi or file an [Issue](https://github.com/aproposorg/cgragen/issues).
