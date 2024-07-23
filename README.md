The Saturn Vector Unit
============================

This repository contains the Chisel source RTL for the Saturn Vector Unit, a parameterized RVV 1.0 vector unit.
Saturn supports the entire RVV 1.0 application-profile specification, including

This README is a temporary placeholder, more in-depth documentation will be released soon.

 * `V`-extension (Full application-profile V)
 * `Zved64` - supports FP64, `ELEN`=64
 * `Zvfh` - supports FP16
 * `Zvl64/128/256/512/1024` - configurable `VLEN`
 * Indexed/strided/segmented loads and stores
 * Virtual memory with precise traps
 * Full chaining with zero dead-time
 * Configurable SIMD datapath width (64/128/256/512+)