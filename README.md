The Saturn Vector Unit
====================================

<img src="docs/diag/overview.png">

Microarchitectural manual [here](https://saturn-vectors.org/).

This repository contains the Chisel source RTL for the Saturn Vector Unit, a parameterized RVV 1.0 vector unit.
Saturn supports the entire RVV 1.0 application-profile specification, including

 * `V`-extension (Full application-profile V)
 * `Zve64d` - supports FP64, `ELEN`=64
 * `Zvfh` - supports FP16
 * `Zvbb` - support basic vector bit manipulation
 * `Zvl64/128/256/512/1024` - configurable `VLEN`
 * Indexed/strided/segmented loads and stores
 * Virtual memory with precise traps
 * Full chaining with zero dead-time
 * Configurable SIMD datapath width (64/128/256/512+)

Using Saturn
-------------

This repository cannot be used stand-alone.
Saturn is intended to be used to generate vector-enabled RISC-V cores and SoCs through the [Chipyard](https://github.com/ucb-bar/chipyard) SoC design framework.


Citing Saturn
--------------

```
@techreport{Zhao:EECS-2024-215,
    Author = {Zhao, Jerry and Grubb, Daniel and Rusch, Miles and Wei, Tianrui and Anderson, Kevin and Nikolic, Borivoje and Asanović, Krste},
    Title = {The Saturn Microarchitecture Manual},
    Institution = {EECS Department, University of California, Berkeley},
    Year = {2024},
    Month = {Dec},
    URL = {http://www2.eecs.berkeley.edu/Pubs/TechRpts/2024/EECS-2024-215.html},
    Number = {UCB/EECS-2024-215}
}
```